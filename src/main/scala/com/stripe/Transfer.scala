package com.stripe

import com.stripe
import com.stripe._

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


case class TransferCollection(count: Int, data: List[Transfer])

case class TransferSummary(
    adjustmentCount: Int,
    adjustmentGross: Int,
    refundGross: Int,
    net: Int,
    refundFees: Int,
    chargeGross: Int,
    validationCount: Int,
    validationFees: Int,
    chargeFees: Int,
    refundCount: Int,
    chargeCount: Int,
    chargeFeeDetails: ChargeFeeDetails)

case class ChargeFeeDetails(
    amount: Int,
    currency: String,
    `type`: String,
    application: Option[String])

case class Transfer(
  status: String,
  otherTransfers: List[String],
  amount: Int,
  date: Int,
  summary: scala.collection.immutable.HashMap[String,AnyRef],
  description: Option[String],
  id: Option[Int],
  `object`: String)
  
object Transfer extends APIResource {

  def retrieve(id: String): Transfer = {
    return request("GET", instanceURL(id)).extract[Transfer]
  }

  def all(params: Map[String,_] = Map.empty): TransferCollection = {
    return request("GET", classURL, params).extract[TransferCollection]
  }
}

