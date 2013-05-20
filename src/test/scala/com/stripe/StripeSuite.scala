package com.stripe

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import java.util.UUID

trait StripeSuite extends ShouldMatchers {
  //set the stripe API key
  apiKey = "tGN0bIwXnHdwOa85VABjPdSn8nWY7G7I"

  val DefaultCardMap = Map(
    "name" -> "Scala User",
    "cvc" -> "100",
    "address_line1" -> "12 Main Street",
    "address_line2" -> "Palo Alto",
    "address_zip" -> "94105",
    "address_country" -> "USA",
    "number" -> "4242424242424242",
    "exp_month" -> 3,
    "exp_year" -> 2015)

  val DefaultChargeMap = Map("amount" -> 100, "currency" -> "usd", "card" -> DefaultCardMap)

  val DefaultCustomerMap = Map("description" -> "Scala Customer", "card" -> DefaultCardMap)

  val DefaultPlanMap = Map("amount" -> 100, "currency" -> "usd", "interval" -> "month", "name" -> "Scala Plan")

  def getUniquePlanId(): String = return "PLAN-%s".format(UUID.randomUUID())

  def getUniquePlanMap(): Map[String,_] = return DefaultPlanMap + ("id" -> getUniquePlanId())

  val DefaultInvoiceItemMap = Map("amount" -> 100, "currency" -> "usd")

  def getUniqueCouponMap(): Map[String,_] = Map("id" -> "COUPON-%s".format(UUID.randomUUID()),
    "duration" -> "once",
    "percent_off" -> 10
  )
}

class ChargeSuite extends FunSuite with StripeSuite {
  test("Charges can be created") {
    val charge = Charge.create(Map("amount" -> 100, "currency" -> "usd", "card" -> DefaultCardMap))
    charge.refunded should be (false)
  }

  test("Charges can be retrieved individually") {
    val createdCharge = Charge.create(DefaultChargeMap)
    val retrievedCharge = Charge.retrieve(createdCharge.id)
    createdCharge.created should equal (retrievedCharge.created)
  }

  test("Charges can be refunded") {
    val charge = Charge.create(DefaultChargeMap)
    val refundedCharge = charge.refund()
    refundedCharge.refunded should equal (true)
  }

  test("Charges can be listed") {
    val charge = Charge.create(DefaultChargeMap)
    val charges = Charge.all().data
    charges.head.isInstanceOf[Charge] should be (true)
  }

  test("Invalid card raises CardException") {
    val e = intercept[CardException] {
      Charge.create(Map(
        "amount" -> 100,
        "currency" -> "usd",
        "card" -> Map("number" -> "4242424242424241", "exp_month" -> 3, "exp_year" -> 2015)
      ))
    }
    e.param.get should equal ("number")
  }

  test("CVC, address and zip checks should pass in testmode") {
    val charge = Charge.create(DefaultChargeMap)
    charge.card.cvcCheck.get should equal ("pass")
    charge.card.addressLine1Check.get should equal ("pass")
    charge.card.addressZipCheck.get should equal ("pass")
  }
}

class CustomerSuite extends FunSuite with StripeSuite {
  test("Customers can be created") {
    val customer = Customer.create(DefaultCustomerMap + ("description" -> "Test Description"))
    customer.description.get should be ("Test Description")
    customer.activeCard.isEmpty should be (false)
  }

  test("Customers can be retrieved individually") {
    val createdCustomer = Customer.create(DefaultCustomerMap)
    val retrievedCustomer = Customer.retrieve(createdCustomer.id)
    createdCustomer.created should equal (retrievedCustomer.created)
  }

  test("Customers can be updated") {
    val customer = Customer.create(DefaultCustomerMap)
    val updatedCustomer = customer.update(Map("description" -> "Updated Scala Customer"))
    updatedCustomer.description.get should equal ("Updated Scala Customer")
  }

  test("Customers can be deleted") {
    val customer = Customer.create(DefaultCustomerMap)
    val deletedCustomer = customer.delete()
    deletedCustomer.deleted should be (true)
    deletedCustomer.id should equal (customer.id)
  }

  test("Customers can be listed") {
    val customer = Customer.create(DefaultCustomerMap)
    val customers = Customer.all().data
    customers.head.isInstanceOf[Customer] should be (true)
  }
}

class PlanSuite extends FunSuite with StripeSuite {
  test("Plans can be created") {
    val plan = Plan.create(getUniquePlanMap + ("interval" -> "year"))
    plan.interval should equal ("year")
  }

  test("Plans can be retrieved individually") {
    val createdPlan = Plan.create(getUniquePlanMap)
    val retrievedPlan = Plan.retrieve(createdPlan.id)
    createdPlan should equal (retrievedPlan)
  }

  test("Plans can be deleted") {
    val plan = Plan.create(getUniquePlanMap)
    val deletedPlan = plan.delete()
    deletedPlan.deleted should be (true)
    deletedPlan.id should equal (plan.id)
  }

  test("Plans can be listed") {
    val plan = Plan.create(getUniquePlanMap)
    val plans = Plan.all().data
    plans.head.isInstanceOf[Plan] should be (true)
  }

  test("Customers can be created with a plan") {
    val plan = Plan.create(getUniquePlanMap)
    val customer = Customer.create(DefaultCustomerMap + ("plan" -> plan.id))
    customer.subscription.get.plan.id should equal (plan.id)
  }

  test("A plan can be added to a customer without a plan") {
    val customer = Customer.create(DefaultCustomerMap)
    val plan = Plan.create(getUniquePlanMap)
    val subscription = customer.updateSubscription(Map("plan" -> plan.id))
    subscription.customer should equal (customer.id)
    subscription.plan.id should equal (plan.id)
  }

  test("A customer's existing plan can be replaced") {
    val origPlan = Plan.create(getUniquePlanMap)
    val customer = Customer.create(DefaultCustomerMap + ("plan" -> origPlan.id))
    customer.subscription.get.plan.id should equal (origPlan.id)
    val newPlan = Plan.create(getUniquePlanMap)
    val subscription = customer.updateSubscription(Map("plan" -> newPlan.id))
    val updatedCustomer = Customer.retrieve(customer.id)
    updatedCustomer.subscription.get.plan.id should equal (newPlan.id)
  }

  test("Customer subscriptions can be canceled") {
    val plan = Plan.create(getUniquePlanMap)
    val customer = Customer.create(DefaultCustomerMap + ("plan" -> plan.id))
    customer.subscription.get.status should equal ("active")
    val canceledSubscription = customer.cancelSubscription()
    canceledSubscription.status should be ("canceled")
  }
}

class InvoiceItemSuite extends FunSuite with StripeSuite {
  def createDefaultInvoiceItem(): InvoiceItem = {
    val customer = Customer.create(DefaultCustomerMap)
    return InvoiceItem.create(DefaultInvoiceItemMap + ("customer" -> customer.id))
  }

  test("InvoiceItems can be created") {
    val invoiceItem = createDefaultInvoiceItem()
    invoiceItem.date should be > (0L)
  }

  test("InvoiceItems can be retrieved individually") {
    val createdInvoiceItem = createDefaultInvoiceItem()
    val retrievedInvoiceItem = InvoiceItem.retrieve(createdInvoiceItem.id)
    createdInvoiceItem.date should equal (retrievedInvoiceItem.date)
  }

  test("InvoiceItems can be updated") {
    val invoiceItem = createDefaultInvoiceItem()
    val updatedInvoiceItem = invoiceItem.update(Map(
      "amount" -> 200, "description" -> "Updated Scala InvoiceItem"
    ))
    updatedInvoiceItem.amount should equal (200)
    updatedInvoiceItem.description.get should equal ("Updated Scala InvoiceItem")
  }

  test("InvoiceItems can be deleted") {
    val invoiceItem = createDefaultInvoiceItem()
    val deletedInvoiceItem = invoiceItem.delete()
    deletedInvoiceItem.deleted should be (true)
    deletedInvoiceItem.id should equal (invoiceItem.id)
  }

  test("InvoiceItems can be listed") {
    val invoiceItem = createDefaultInvoiceItem()
    val invoiceItems = InvoiceItem.all().data
    invoiceItems.head.isInstanceOf[InvoiceItem] should be (true)
  }
}

class InvoiceSuite extends FunSuite with StripeSuite {
  test("Invoices can be retrieved individually") {
    val plan = Plan.create(getUniquePlanMap)
    val customer = Customer.create(DefaultCustomerMap + ("plan" -> plan.id))
    val invoices = Invoice.all(Map("customer" -> customer.id)).data
    val createdInvoice = invoices.head
    val retrievedInvoice = Invoice.retrieve(createdInvoice.id.get)
    retrievedInvoice.id should equal (createdInvoice.id)
  }

  test("Invoices can be listed") {
    val plan = Plan.create(getUniquePlanMap)
    val customer = Customer.create(DefaultCustomerMap + ("plan" -> plan.id))
    val invoices = Invoice.all().data
    invoices.head.isInstanceOf[Invoice] should be (true)
  }

  test("Invoices can be retrieved for a customer") {
    val plan = Plan.create(getUniquePlanMap)
    val customer = Customer.create(DefaultCustomerMap + ("plan" -> plan.id))
    val invoices = Invoice.all(Map("customer" -> customer.id)).data
    val invoice = invoices.head
    invoice.customer should equal (customer.id)
    val invoiceLineSubscription = invoice.lines.data.head
    invoiceLineSubscription.plan.map(_.id) should equal (Some(plan.id))
  }

  test("Upcoming Invoices can be retrieved") {
    val customer = Customer.create(DefaultCustomerMap)
    val customerId = customer.id
    val invoiceItem = InvoiceItem.create(DefaultInvoiceItemMap + ("customer" -> customerId))
    val upcomingInvoice = Invoice.upcoming(Map("customer" -> customerId))
//    upcomingInvoice.attempted should be (false)
  }
}

class TokenSuite extends FunSuite with StripeSuite {
  test("Tokens can be created") {
    val token = Token.create(Map("card" -> DefaultCardMap))
    token.used should be (false)
  }

  test("Tokens can be retrieved") {
    val createdToken = Token.create(Map("card" -> DefaultCardMap))
    val retrievedToken = Token.retrieve(createdToken.id)
    createdToken.created should equal (retrievedToken.created)
  }

  test("Tokens can be used") {
    val createdToken = Token.create(Map("card" -> DefaultCardMap))
    createdToken.used should be (false)
    val charge = Charge.create(Map("amount" -> 100, "currency" -> "usd", "card" -> createdToken.id))
    val retrievedToken = Token.retrieve(createdToken.id)
    retrievedToken.used should equal (true)
  }
}

class CouponSuite extends FunSuite with StripeSuite {
  test("Coupons can be created") {
    val coupon = Coupon.create(getUniqueCouponMap)
    coupon.percentOff should equal (10)
  }

  test("Coupons can be retrieved individually") {
    val createdCoupon = Coupon.create(getUniqueCouponMap)
    val retrievedCoupon = Coupon.retrieve(createdCoupon.id)
    createdCoupon should equal (retrievedCoupon)
  }

  test("Coupons can be deleted") {
    val coupon = Coupon.create(getUniqueCouponMap)
    val deletedCoupon = coupon.delete()
    deletedCoupon.deleted should be (true)
    deletedCoupon.id should equal (coupon.id)
  }

  test("Coupons can be listed") {
    val coupon = Coupon.create(getUniqueCouponMap)
    val coupons = Coupon.all().data
    coupons.head.isInstanceOf[Coupon] should be (true)
  }
}

class AccountSuite extends FunSuite with StripeSuite {
  test("Account can be retrieved") {
    val account = Account.retrieve
    account.email should equal (Some("test+bindings@stripe.com"))
    account.chargeEnabled should equal (false)
    account.detailsSubmitted should be (false)
    account.statementDescriptor should be (None)
    account.currenciesSupported.length should be (1)
    account.currenciesSupported.head should be ("USD")
  }
}
