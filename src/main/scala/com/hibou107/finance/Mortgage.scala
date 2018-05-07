package com.hibou107.finance




class Mortgage(capital: Double, durationMonths: Int, interestRate: Double, insurance: Double) extends Instrument {
  case class MonthPret(monthNum: Int, captitalReimbursement: Double, interest: Double, remainedCapital: Double, insurance: Double, totalPayment: Double)

  override def cashflows(maturityMonth: Int): Vector[Cashflow] = {
    val monthlyInsurance = capital * insurance / 12.0
    val monthlyRate = interestRate / 12.0
    val monthlyPayment = Mortgage.computeMensualite(capital, interestRate, durationMonths)

    val paymentCashflows = {
      val zero = MonthPret(0, 0, 0, Tools.floor(capital), 0.0, 0.0)
      (1 to durationMonths).scanLeft(zero) { case (lastMonth, currentMonth) =>
        val interestPayment = lastMonth.remainedCapital * monthlyRate
        val reimbursedCapital = monthlyPayment - interestPayment
        val currentCapitalRemained = lastMonth.remainedCapital - reimbursedCapital
        val totalPayment = monthlyInsurance + monthlyPayment
        MonthPret(currentMonth, reimbursedCapital, interestPayment, currentCapitalRemained, monthlyInsurance, totalPayment)
      }
    }.toVector

    (0 to maturityMonth).map {
      case i if i == 0 =>  MortgageReceive(capital)
      case i if i <= maturityMonth =>
        val flow = paymentCashflows(i)
        MortgagePayment(flow.totalPayment, flow.interest, flow.insurance, flow.captitalReimbursement)
      case _ => Zero
    }.toVector
  }
}


object Mortgage {

  def computeMensualite(K: Double, t: Double, nbMonths: Int) = {
    (K * t / 12.0) / (1 - Math.pow(1 + t / 12.0, -nbMonths))
  }


}