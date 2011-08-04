package com.stripe

import java.net.URLEncoder

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

import org.apache.commons.codec.binary.Base64
import org.apache.http.client._
import org.apache.http.impl.client._
import org.apache.http.client.methods._
import org.apache.http.client.params._
import org.apache.http.client.entity._
import org.apache.http.params._
import org.apache.http.message._
import org.apache.http.util._

import net.liftweb.json

sealed abstract class StripeException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
case class APIException(msg: String, cause: Throwable = null) extends StripeException(msg, cause)
case class APIConnectionException(msg: String, cause: Throwable = null) extends StripeException(msg, cause)
case class CardException(msg: String, code: Option[String] = None, param: Option[String] = None) extends StripeException(msg)
case class InvalidRequestException(msg: String, param: Option[String] = None) extends StripeException(msg)
case class AuthenticationException(msg: String) extends StripeException(msg)

abstract class APIResource {
  val ApiBase = "https://api.stripe.com/v1"
  val Version = "1.0.0"
  val CharSet = "UTF-8"

  //lift-json format initialization
  implicit val formats = json.DefaultFormats

  //utility methods
  def base64(in: String) = new String(Base64.encodeBase64(in.getBytes(CharSet)))
  def urlEncodePair(k:String, v: String) = "%s=%s".format(URLEncoder.encode(k, CharSet), URLEncoder.encode(v, CharSet))
  val className = this.getClass.getSimpleName.toLowerCase.replace("$","")
  val classURL = "%s/%ss".format(ApiBase, className)
  def instanceURL(id: String) = "%s/%s".format(classURL, id)
  val camelCaseRegex = new Regex("(_.)")

  def createParameterList(paramMap: Map[String,_]): ListBuffer[(String, String)] = {
    /*
        We want POST vars of form:
        {'foo': 'bar', 'nested': {'a': 'b', 'c': 'd'}}
        to become:
        foo=bar&nested[a]=b&nested[c]=d
    */
    val paramList = new ListBuffer[(String, String)]
    if (paramMap.isEmpty) return paramList
    for ((key, value) <- paramMap) {
      value match {
        case None => //noop
        case m: Map[_,_] => {
          val flatMap = m.map(kv => ("%s[%s]".format(key,kv._1), kv._2))
          paramList ++= createParameterList(flatMap)
        }
        case _ => paramList.append((key, value.toString))
      }
    }
    return paramList
  }

  def rawRequest(method: String, url: String, params: Map[String,_] = Map.empty): (String, Int) = {
    if (apiKey == null || apiKey.isEmpty) {
      throw AuthenticationException("No API key provided. (HINT: set your API key using 'stripe.apiKey = <API-KEY>'. You can generate API keys from the Stripe web interface. See https://stripe.com/api for details or email support@stripe.com if you have questions.")
    }
    val defaultHeaders = asJavaCollection(List(
      new BasicHeader("X-Stripe-Client-User-Agent", "StripeScala"),
      new BasicHeader("User-Agent", "Stripe/v1 ScalaBindings/%s".format(Version)),
      new BasicHeader("Authorization", "Basic %s".format(base64("%s:".format(apiKey))))
    ))

    val httpParams = new SyncBasicHttpParams().
      setParameter(ClientPNames.DEFAULT_HEADERS, defaultHeaders).
      setParameter(CoreProtocolPNames.HTTP_CONTENT_CHARSET,CharSet). //30 seconds
      setParameter(CoreConnectionPNames.CONNECTION_TIMEOUT,30000). //30 seconds
      setParameter(CoreConnectionPNames.SO_TIMEOUT,80000) //80 seconds

    val client = new DefaultHttpClient(httpParams)
    val paramList = createParameterList(params)
    try {
      val response = method.toLowerCase match {
        case "get" => {
          val getURL = "%s?%s".format(url, paramList.map(kv => urlEncodePair(kv._1, kv._2)).mkString("&"))
          client.execute(new HttpGet(getURL))
        }
        case "delete" => {
          client.execute(new HttpDelete(url))
        }
        case "post" => {
          val request = new HttpPost(url)
          val postParamList = paramList.map(kv => new BasicNameValuePair(kv._1, kv._2))
          request.setEntity(new UrlEncodedFormEntity(seqAsJavaList(postParamList), CharSet))
          client.execute(request)
        }
        case _ => throw new APIConnectionException(
          "Unrecognized HTTP method %r. This may indicate a bug in the Stripe bindings. Please contact support@stripe.com for assistance.".format(method))
      }
      val entity = response.getEntity
      val body = EntityUtils.toString(entity)
      EntityUtils.consume(entity)
      return (body, response.getStatusLine.getStatusCode)
    } catch {
      case e @ (_: java.io.IOException | _: ClientProtocolException) => throw APIConnectionException("Could not connect to Stripe (%s). Please check your internet connection and try again. If this problem persists, you should check Stripe's service status at https://twitter.com/stripe, or let us know at support@stripe.com.".format(ApiBase), e)
    } finally {
      client.getConnectionManager.shutdown()
    }
  }

  def request(method: String, url: String, params: Map[String,_] = Map.empty): json.JValue = {
    val (rBody, rCode) = rawRequest(method, url, params)
    val jsonAST = interpretResponse(rBody, rCode)
    return jsonAST
  }

  def interpretResponse(rBody: String, rCode: Int): json.JValue = {
    val jsonAST = json.parse(rBody).transform {
      //converts json camel_case field names to Scala camelCase field names
      case json.JField(fieldName, x) => json.JField(camelCaseRegex.replaceAllIn(
        fieldName, (m: Regex.Match) => m.matched.substring(1).toUpperCase), x)
    }
    if (rCode < 200 || rCode >= 300) handleAPIError(rBody, rCode, jsonAST)
    return jsonAST
  }

  def handleAPIError(rBody: String, rCode: Int, jsonAST: json.JValue) {
    val error = try {
       jsonAST.extract[ErrorContainer].error
    } catch {
      case e: json.MappingException => throw new APIException(
        "Unable to parse response body from API: %s (HTTP response code was %s)".format(rBody, rCode), e)
    }
    rCode match {
      case (400 | 404) => throw new InvalidRequestException(error.message, param=error.param)
      case 401 => throw new AuthenticationException(error.message)
      case 402 => throw new CardException(error.message, code=error.code, param=error.param)
      case _ => throw new APIException(error.message, null)
    }
  }
}

//represents Errors returned as JSON
case class ErrorContainer(error: Error)
case class Error(`type`: String, message: String, code: Option[String], param: Option[String])

case class Card(
  expMonth: Int,
  expYear: Int,
  last4: String,
  country: String,
  `object`: String,
  `type`: String,
  name: Option[String] = None,
  addressLine1: Option[String] = None,
  addressLine2: Option[String] = None,
  addressZip: Option[String] = None,
  addressState: Option[String] = None,
  addressCountry: Option[String] = None) extends APIResource

case class Charge(
  amount: Int,
  created: Int,
  currency: String,
  id: String,
  livemode: Boolean,
  `object`: String,
  paid: Boolean,
  refunded: Boolean,
  card: Card) extends APIResource {
  def refund(): Charge = request("POST", "%s/refund".format(instanceURL(this.id))).extract[Charge]
}

object Charge extends APIResource {
  def create(params: Map[String,_]): Charge = {
    return request("POST", classURL, params).extract[Charge]
  }

  def all(params: Map[String,_] = Map.empty): List[Charge] = {
    return request("GET", classURL, params).extract[List[Charge]]
  }

  def retrieve(id: String): Charge = {
    return request("GET", instanceURL(id)).extract[Charge]
  }
}
case class Customer(
  id: String,
  created: Long,
  livemode: Boolean,
  `object`: String,
  activeCard: Option[Card],
  email: Option[String],
  description: Option[String],
  plan: Option[String],
  trialEnd: Option[Long],
  discount: Option[Discount],
  nextRecurringCharge: Option[NextRecurringCharge],
  subscription: Option[Subscription]
) extends APIResource {
  def update(params: Map[String,_]): Customer = {
    return request("POST", instanceURL(this.id), params).extract[Customer]
  }

  def delete(): DeletedCustomer = {
    return request("DELETE", instanceURL(this.id)).extract[DeletedCustomer]
  }

  def updateSubscription(params: Map[String,_]): Subscription = {
    return request("POST", "%s/subscription".format(instanceURL(id)), params).extract[Subscription]
  }

  def cancelSubscription(params: Map[String,_] = Map.empty): Subscription = {
    return request("DELETE", "%s/subscription".format(instanceURL(id)), params).extract[Subscription]
  }
}

case class DeletedCustomer(id: String, deleted: Boolean)

object Customer extends APIResource {
  def create(params: Map[String,_]): Customer = {
    return request("POST", classURL, params).extract[Customer]
  }

  def retrieve(id: String): Customer = {
    return request("GET", instanceURL(id)).extract[Customer]
  }

  def all(params: Map[String,_] = Map.empty): List[Customer] = {
    return request("GET", classURL, params).extract[List[Customer]]
  }
}

case class Plan(
  amount: Int,
  currency: String,
  id: String,
  interval: String,
  name: String,
  `object`: String,
  trialPeriodDays: Option[Int]) extends APIResource {
  def delete(): DeletedPlan = {
    return request("DELETE", instanceURL(this.id)).extract[DeletedPlan]
  }
}

case class DeletedPlan(id: String, deleted: Boolean)

object Plan extends APIResource {
  def create(params: Map[String,_]): Plan = {
    return request("POST", classURL, params).extract[Plan]
  }

  def retrieve(id: String): Plan = {
    return request("GET", instanceURL(id)).extract[Plan]
  }

  def all(params: Map[String,_] = Map.empty): List[Plan] = {
    return request("GET", classURL, params).extract[List[Plan]]
  }
}

case class Subscription(
  currentPeriodEnd: Long,
  currentPeriodStart: Long,
  customer: String,
  `object`: String,
  start: Long,
  status: String,
  trialStart: Option[Long],
  trialEnd: Option[Long],
  plan: Plan
)

case class NextRecurringCharge(amount: Int, date: String)
case class Discount(code: String, end: Long, id: String, `object`: String, percentOff: Int, start: Long)
