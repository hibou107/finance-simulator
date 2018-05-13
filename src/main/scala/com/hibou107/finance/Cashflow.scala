package com.hibou107.finance


sealed trait Cashflow {
  val value: Double
}

case class MultiCashFlow(values: List[UnitCashflow]) extends Cashflow {
  val value: Double = values.map(_.value).sum
}

sealed trait UnitCashflow extends Cashflow

case class RentalIncome(value: Double) extends UnitCashflow // all values are postives
case class SalaryIncome(value: Double) extends UnitCashflow
case class MortgageReceive(value: Double) extends UnitCashflow

case class SCPISell(value: Double, originValue: Double) extends UnitCashflow

case class RentPayment(value: Double) extends UnitCashflow
case class LifePayment(value: Double) extends UnitCashflow
case class TaxPayment(value: Double) extends UnitCashflow

case class MortgagePayment(value: Double, interest: Double, insurance: Double, capitalReimbursement: Double) extends UnitCashflow // all values are negatives
case class Payment(value: Double) extends UnitCashflow // negative value

case class IRPayment(value: Double) extends UnitCashflow

case object Zero extends UnitCashflow {
  val value: Double = 0.0
}