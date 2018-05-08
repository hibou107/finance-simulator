package com.hibou107.finance


sealed trait Cashflow {
  val value: Double
}

case class ComposeCashFlow(values: List[Cashflow]) extends Cashflow {
  val value: Double = values.map(_.value).sum
}
case class RentalIncome(value: Double) extends Cashflow // all values are postives
case class SalaryIncome(value: Double) extends Cashflow
case class MortgageReceive(value: Double) extends Cashflow

case class RentPayment(value: Double) extends Cashflow
case class LifePayment(value: Double) extends Cashflow

case class MortgagePayment(value: Double, interest: Double, insurance: Double, capitalReimbursement: Double) extends Cashflow // all values are negatives

case class Payment(value: Double) extends Cashflow // negative value
case object Zero extends Cashflow {
  val value: Double = 0.0
}