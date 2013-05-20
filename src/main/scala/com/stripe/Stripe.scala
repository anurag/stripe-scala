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
import net.liftweb.json.JsonAST

sealed abstract class StripeException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
case class APIException(msg: String, cause: Throwable = null) extends StripeException(msg, cause)
case class APIConnectionException(msg: String, cause: Throwable = null) extends StripeException(msg, cause)
case class CardException(msg: String, code: Option[String] = None, param: Option[String] = None) extends StripeException(msg)
case class InvalidRequestException(msg: String, param: Option[String] = None) extends StripeException(msg)
case class AuthenticationException(msg: String) extends StripeException(msg)

abstract class APIResource {
  val ApiBase = "https://api.stripe.com/v1"
  val BindingsVersion = "1.1.3"
  val CharSet = "UTF-8"

  //lift-json format initialization
  implicit val formats = json.DefaultFormats

  //utility methods
  def base64(in: String) = new String(Base64.encodeBase64(in.getBytes(CharSet)))
  def urlEncodePair(k:String, v: String) = "%s=%s".format(URLEncoder.encode(k, CharSet), URLEncoder.encode(v, CharSet))
  val className = this.getClass.getSimpleName.toLowerCase.replace("$","")
  val classURL = "%s/%ss".format(ApiBase, className)
  def instanceURL(id: String) = "%s/%s".format(classURL, id)
  val singleInstanceURL = "%s/%s".format(ApiBase, className)

  /*
      We want POST vars of form:
      {'foo': 'bar', 'nested': {'a': 'b', 'c': 'd'}}
      to become:
      foo=bar&nested[a]=b&nested[c]=d
  */
  def flattenParam(k: String, v: Any): List[(String, String)] = {
    v match {
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

  def deleteRequest(url: String): HttpRequestBase = new HttpDelete(url)

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
    interpretResponse(rBody, rCode)
  }

  val CamelCaseRegex = new Regex("(_.)")

  def interpretResponse(rBody: String, rCode: Int): json.JValue = {
    val jsonAST = json.parse(rBody).transform {
      //converts json camel_case field names to Scala camelCase field names
      case json.JField(fieldName, x) => json.JField(CamelCaseRegex.replaceAllIn(
        fieldName, (m: Regex.Match) => m.matched.substring(1).toUpperCase), x)
    }
    if (rCode < 200 || rCode >= 300) handleAPIError(rBody, rCode, jsonAST)
    jsonAST
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
  last4: String,
  `type`: String,
  `object`: String,
  expMonth: Int,
  expYear: Int,
  fingerprint: String,
  country: Option[String],
  name: Option[String] = None,
  addressLine1: Option[String] = None,
  addressLine2: Option[String] = None,
  addressZip: Option[String] = None,
  addressState: Option[String] = None,
  addressCity: Option[String] = None,
  addressCountry: Option[String] = None,
  cvcCheck: Option[String] = None,
  addressLine1Check: Option[String] = None,
  addressZipCheck: Option[String] = None) extends APIResource


case class Charge(
  created: Long,
  id: String,
  `object`: String,
  livemode: Boolean,
  paid: Boolean,
  amount: Int,
  currency: String,
  refunded: Boolean,
  dispute: Option[String] = None,
  fee: Int,
  card: Card,
  captured: Boolean,
  feeDetails: List[FeeDetail],
  failureMessage: Option[String],
  amountRefunded: Option[Int],
  customer: Option[String],
  invoice: Option[String],
  description: Option[String]) extends APIResource {
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

case class FeeDetail(
  amount: Int,
  currency: String,
  `type`: String,
  description: Option[String],
  application: Option[String],
  amountRefunded: Option[Int]
)

case class Customer(
  created: Long,
  id: String,
  livemode: Boolean,
  description: Option[String],
  activeCard: Option[Card],
  email: Option[String],
  delinquent: Option[Boolean],
  subscription: Option[Subscription],
  discount: Option[Discount],
  accountBalance: Option[Int]) extends APIResource {
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
  id: String,
  name: String,
  interval: String,
  intervalCount: Int,
  `object`: String,
  amount: Int,
  currency: String,
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
  plan: Plan,
  start: Long,
  status: String,
  customer: String,
  cancelAtPeriodEnd: Option[Boolean],
  currentPeriodStart: Option[Long],
  currentPeriodEnd: Option[Long],
  endedAt: Option[Long],
  trialStart: Option[Long],
  trialEnd: Option[Long],
  canceledAt: Option[Long]) extends APIResource {
}

case class NextRecurringCharge(amount: Int, date: String)

case class Discount(
  id: String,
  coupon: String,
  start: Long,
  customer: String,
  end: Option[Long]) extends APIResource {
}

case class InvoiceItem(
  id: String,
  amount: Int,
  currency: String,
  date: Long,
  livemode: Boolean,
  description: Option[String],
  invoice: Option[Invoice]) extends APIResource {
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
  `object`: String,
  count: Int,
  url: String,
  data: List[InvoiceLine]
)

case class InvoiceLine(
  id: String,
  `object`: String,
  `type`: String,
  livemode: Boolean,
  amount: Int,
  currency: String,
  proration: Boolean,
  period: InvoiceLineSubscriptionPeriod,
  quantity: Option[Int],
  plan: Option[Plan],
  description: Option[String]
)

case class Invoice(
  date: Long,
  // id is optional since UpcomingInvoices don't have an ID.
  id: Option[String],
  periodStart: Long,
  periodEnd: Long,
  lines: InvoiceLines,
  subtotal: Int,
  total: Int,
  customer: String,
  `object`: String,
  attempted: Boolean,
  closed: Boolean,
  paid: Boolean,
  livemode: Boolean,
  attemptCount: Int,
  amountDue: Int,
  currency: String,
  startingBalance: Int,
  endingBalance: Option[Int],
  nextPaymentAttempt: Option[Long],
  charge: Option[String],
  discount: Option[Discount]) {
}

case class InvoiceCollection(`object`: String, count: Int, url: String, data: List[Invoice])

object Invoice extends APIResource {
  def retrieve(id: String): Invoice = {
    request("GET", instanceURL(id)).extract[Invoice]
  }

  def all(params: Map[String,_] = Map.empty): InvoiceCollection = {
    request("GET", classURL, params).extract[InvoiceCollection]
  }

  def upcoming(params: Map[String, _]): Invoice = {
    request("GET", "%s/upcoming".format(classURL), params).extract[Invoice]
  }
}

case class Token(
  id: String,
  created: Int,
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
  percentOff: Int,
  livemode: Boolean,
  duration: String,
  redeemBy: Option[Long],
  maxRedemptions: Option[Int],
  timesRedeemed: Option[Int],
  durationInMonths: Option[Int]) extends APIResource {
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

case class Account(
  id: String,
  email: Option[String],
  statementDescriptor: Option[String],
  detailsSubmitted: Boolean,
  chargeEnabled: Boolean,
  currenciesSupported: Array[String]
) extends APIResource

object Account extends APIResource {
  def retrieve: Account = {
    request("GET", singleInstanceURL).extract[Account]
  }
}
