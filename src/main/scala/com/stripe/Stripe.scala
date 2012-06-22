package com.stripe

import java.net.URLEncoder

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.Properties

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
import net.liftweb.json.JsonDSL._

sealed abstract class StripeException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
case class APIException(msg: String, cause: Throwable = null) extends StripeException(msg, cause)
case class APIConnectionException(msg: String, cause: Throwable = null) extends StripeException(msg, cause)
case class CardException(msg: String, code: Option[String] = None, param: Option[String] = None) extends StripeException(msg)
case class InvalidRequestException(msg: String, param: Option[String] = None) extends StripeException(msg)
case class AuthenticationException(msg: String) extends StripeException(msg)

abstract class APIResource {
  val ApiBase = "https://api.stripe.com/v1"
  val BindingsVersion = "1.0.6"
  val CharSet = "UTF-8"

  //lift-json format initialization
  implicit val formats = json.DefaultFormats

  //utility methods
  def base64(in: String) = new String(Base64.encodeBase64(in.getBytes(CharSet)))
  def urlEncodePair(k:String, v: String) = "%s=%s".format(URLEncoder.encode(k, CharSet), URLEncoder.encode(v, CharSet))
  val className = this.getClass.getSimpleName.toLowerCase.replace("$","")
  val classURL = "%s/%ss".format(ApiBase, className)
  def instanceURL(id: String) = "%s/%s".format(classURL, id)

  /*
      We want POST vars of form:
      {'foo': 'bar', 'nested': {'a': 'b', 'c': 'd'}}
      to become:
      foo=bar&nested[a]=b&nested[c]=d
  */
  def flattenParam(k: String, v: Any): List[(String, String)] = {
    return v match {
      case None => Nil
      case m: Map[_,_] => m.flatMap(kv => flattenParam("%s[%s]".format(k,kv._1), kv._2)).toList
      case _ => List((k,v.toString))
    }
  }

  def httpClient: DefaultHttpClient = {
    if (apiKey == null || apiKey.isEmpty) {
      throw AuthenticationException("No API key provided. (HINT: set your API key using 'stripe.apiKey = <API-KEY>'. You can generate API keys from the Stripe web interface. See https://stripe.com/api for details or email support@stripe.com if you have questions.")
    }

    //debug headers
    val javaPropNames = List("os.name", "os.version", "os.arch", "java.version", "java.vendor", "java.vm.version", "java.vm.vendor")
    val javaPropMap = javaPropNames.map(n => (n.toString, Properties.propOrEmpty(n).toString)).toMap
    val fullPropMap = javaPropMap + (
      "scala.version" -> Properties.scalaPropOrEmpty("version.number"),
      "bindings.version" -> BindingsVersion,
      "lang" -> "scala",
      "publisher" -> "stripe"
    )

    val defaultHeaders = asJavaCollection(List(
      new BasicHeader("X-Stripe-Client-User-Agent", json.compact(json.render(fullPropMap))),
      new BasicHeader("User-Agent", "Stripe/v1 ScalaBindings/%s".format(BindingsVersion)),
      new BasicHeader("Authorization", "Bearer %s".format(apiKey))
    ))

    val httpParams = new SyncBasicHttpParams().
      setParameter(ClientPNames.DEFAULT_HEADERS, defaultHeaders).
      setParameter(CoreProtocolPNames.HTTP_CONTENT_CHARSET,CharSet).
      setParameter(CoreConnectionPNames.CONNECTION_TIMEOUT,30000). //30 seconds
      setParameter(CoreConnectionPNames.SO_TIMEOUT,80000) //80 seconds

    new DefaultHttpClient(connectionManager, httpParams)
  }

  def getRequest(url: String, paramList: List[(String,String)]): HttpRequestBase = {
    new HttpGet("%s?%s".format(url, paramList.map(kv => urlEncodePair(kv._1, kv._2)).mkString("&")))
  }

  def deleteRequest(url: String): HttpRequestBase = { return new HttpDelete(url) }

  def postRequest(url: String, paramList: List[(String, String)]): HttpRequestBase = {
    val request = new HttpPost(url)
    val postParamList = paramList.map(kv => new BasicNameValuePair(kv._1, kv._2))
    request.setEntity(new UrlEncodedFormEntity(seqAsJavaList(postParamList), CharSet))
    request
  }

  def rawRequest(method: String, url: String, params: Map[String,_] = Map.empty): (String, Int) = {
    val client = httpClient
    val paramList = params.flatMap(kv => flattenParam(kv._1, kv._2)).toList
    try {
      val request = method.toLowerCase match {
        case "get" => getRequest(url, paramList)
        case "delete" => deleteRequest(url)
        case "post" => postRequest(url, paramList)
        case _ => throw new APIConnectionException("Unrecognized HTTP method %r. This may indicate a bug in the Stripe bindings. Please contact support@stripe.com for assistance.".format(method))
      }
      val response = client.execute(request)
      val entity = response.getEntity
      val body = EntityUtils.toString(entity)
      EntityUtils.consume(entity)
      (body, response.getStatusLine.getStatusCode)
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

  val CamelCaseRegex = new Regex("(_.)")

  def interpretResponse(rBody: String, rCode: Int): json.JValue = {
    val jsonAST = json.parse(rBody).transform {
      //converts json camel_case field names to Scala camelCase field names
      case json.JField(fieldName, x) => json.JField(CamelCaseRegex.replaceAllIn(
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
  `type`: String,
  name: Option[String] = None,
  addressLine1: Option[String] = None,
  addressLine2: Option[String] = None,
  addressZip: Option[String] = None,
  addressState: Option[String] = None,
  addressCountry: Option[String] = None) extends APIResource

case class Charge(
  amount: Int,
  created: Long,
  fee: Int,
  currency: String,
  id: String,
  livemode: Boolean,
  paid: Boolean,
  refunded: Boolean,
  description: Option[String],
  card: Card) extends APIResource {
  def refund(): Charge = request("POST", "%s/refund".format(instanceURL(this.id))).extract[Charge]
}

case class ChargeCollection(count: Int, data: List[Charge])

object Charge extends APIResource {
  def create(params: Map[String,_]): Charge = {
    request("POST", classURL, params).extract[Charge]
  }

  def retrieve(id: String): Charge = {
    request("GET", instanceURL(id)).extract[Charge]
  }

  def all(params: Map[String,_] = Map.empty): ChargeCollection = {
    request("GET", classURL, params).extract[ChargeCollection]
  }

}

case class Customer(
  id: String,
  created: Long,
  livemode: Boolean,
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
    request("POST", instanceURL(this.id), params).extract[Customer]
  }

  def delete(): DeletedCustomer = {
    request("DELETE", instanceURL(this.id)).extract[DeletedCustomer]
  }

  def updateSubscription(params: Map[String,_]): Subscription = {
    request("POST", "%s/subscription".format(instanceURL(id)), params).extract[Subscription]
  }

  def cancelSubscription(params: Map[String,_] = Map.empty): Subscription = {
    request("DELETE", "%s/subscription".format(instanceURL(id)), params).extract[Subscription]
  }
}

case class DeletedCustomer(id: String, deleted: Boolean)

case class CustomerCollection(count: Int, data: List[Customer])

object Customer extends APIResource {
  def create(params: Map[String,_]): Customer = {
    request("POST", classURL, params).extract[Customer]
  }

  def retrieve(id: String): Customer = {
    request("GET", instanceURL(id)).extract[Customer]
  }

  def all(params: Map[String,_] = Map.empty): CustomerCollection = {
    request("GET", classURL, params).extract[CustomerCollection]
  }
}

case class Plan(
  amount: Int,
  currency: String,
  id: String,
  interval: String,
  name: String,
  livemode: Boolean,
  trialPeriodDays: Option[Int]) extends APIResource {
  def delete(): DeletedPlan = {
    request("DELETE", instanceURL(this.id)).extract[DeletedPlan]
  }
}

case class PlanCollection(count: Int, data: List[Plan])

case class DeletedPlan(id: String, deleted: Boolean)

object Plan extends APIResource {
  def create(params: Map[String,_]): Plan = {
    request("POST", classURL, params).extract[Plan]
  }

  def retrieve(id: String): Plan = {
    request("GET", instanceURL(id)).extract[Plan]
  }

  def all(params: Map[String,_] = Map.empty): PlanCollection = {
    request("GET", classURL, params).extract[PlanCollection]
  }
}

case class Subscription(
  currentPeriodEnd: Long,
  currentPeriodStart: Long,
  customer: String,
  start: Long,
  status: String,
  trialStart: Option[Long],
  trialEnd: Option[Long],
  plan: Plan
)

case class NextRecurringCharge(amount: Int, date: String)

case class Discount(
  code: String,
  end: Option[Long],
  id: Option[String],
  percentOff: Int,
  start: Option[Long]
)

case class InvoiceItem(
  id: String,
  amount: Int,
  currency: String,
  date: Long,
  livemode: Boolean,
  description: Option[String])
extends APIResource {
  def update(params: Map[String,_]): InvoiceItem = {
    request("POST", instanceURL(this.id), params).extract[InvoiceItem]
  }

  def delete(): DeletedInvoiceItem = {
    request("DELETE", instanceURL(this.id)).extract[DeletedInvoiceItem]
  }
}

case class DeletedInvoiceItem(id: String, deleted: Boolean)

case class InvoiceItemCollection(count: Int, data: List[InvoiceItem])

object InvoiceItem extends APIResource {
  def create(params: Map[String,_]): InvoiceItem = {
    request("POST", classURL, params).extract[InvoiceItem]
  }

  def retrieve(id: String): InvoiceItem = {
    request("GET", instanceURL(id)).extract[InvoiceItem]
  }

  def all(params: Map[String,_] = Map.empty): InvoiceItemCollection = {
    request("GET", classURL, params).extract[InvoiceItemCollection]
  }
}

case class InvoiceLineSubscriptionPeriod(start: Long, end: Long)
case class InvoiceLineSubscription(plan: Plan, amount: Int, period: InvoiceLineSubscriptionPeriod)
case class InvoiceLines(
  subscriptions: Option[List[InvoiceLineSubscription]],
  invoiceitems: Option[List[InvoiceItem]]
)

case class Invoice(
  subtotal: Int,
  total: Int,
  id: Option[String],
  created: Option[Long],
  attempted: Option[Boolean],
  charge: Option[String],
  closed: Option[Boolean],
  customer: Option[String],
  date: Option[Long],
  paid: Option[Boolean],
  periodStart: Option[Long],
  periodEnd: Option[Long],
  discount: Option[Discount],
  lines: InvoiceLines) {
}

case class InvoiceCollection(count: Int, data: List[Invoice])

object Invoice extends APIResource {
  def retrieve(id: String): Invoice = {
    return request("GET", instanceURL(id)).extract[Invoice]
  }

  def all(params: Map[String,_] = Map.empty): InvoiceCollection = {
    request("GET", classURL, params).extract[InvoiceCollection]
  }

  def upcoming(params: Map[String, _]): Invoice = {
    request("GET", "%s/upcoming".format(classURL), params).extract[Invoice]
  }
}

case class Token(
  amount: Int,
  created: Int,
  id: String,
  livemode: Boolean,
  used: Boolean,
  card: Card) extends APIResource {
}

object Token extends APIResource {
  def create(params: Map[String,_]): Token = {
    request("POST", classURL, params).extract[Token]
  }

  def retrieve(id: String): Token = {
    request("GET", instanceURL(id)).extract[Token]
  }
}

case class Coupon(
  id: String,
  duration: String,
  livemode: Boolean,
  percentOff: Int) extends APIResource {
  def delete(): DeletedCoupon = {
    request("DELETE", instanceURL(this.id)).extract[DeletedCoupon]
  }
}

case class CouponCollection(count: Int, data: List[Coupon])

case class DeletedCoupon(id: String, deleted: Boolean)

object Coupon extends APIResource {
  def create(params: Map[String,_]): Coupon = {
    request("POST", classURL, params).extract[Coupon]
  }

  def retrieve(id: String): Coupon = {
    request("GET", instanceURL(id)).extract[Coupon]
  }

  def all(params: Map[String,_] = Map.empty): CouponCollection = {
    request("GET", classURL, params).extract[CouponCollection]
  }
}
