package com.hibou107.finance

class Mortgage(capital: Double, durationMonths: Int, interestRate: Double, insurance: Double, reimbursementYield: Double) extends Instrument {



  case class MortgageState(monthNum: Int, captitalReimbursement: Double, interest: Double, capitalPaid: Double,
                           remainedCapital: Double, insurance: Double, totalPayment: Double)
  val monthlyInsurance: Double = capital * insurance / 12.0
  val monthlyRate: Double = interestRate / 12.0
  val monthlyPayment: Double = Mortgage.computeMensualite(capital, interestRate, durationMonths)

  lazy val mortgageTable: Vector[MortgageState] = {
    val zero = MortgageState(0, 0, 0, Tools.floor(capital), 0.0, 0.0, 0.0)
    (1 to durationMonths).scanLeft(zero) { case (lastMonth, currentMonth) =>
      val interestPayment = lastMonth.remainedCapital * monthlyRate
      val reimbursedCapital = monthlyPayment - interestPayment
      val currentCapitalRemained = lastMonth.remainedCapital - reimbursedCapital
      val totalPayment = monthlyInsurance + monthlyPayment
      val currentCapitalPaid = lastMonth.capitalPaid + reimbursedCapital
      MortgageState(currentMonth, reimbursedCapital, interestPayment, currentCapitalPaid, currentCapitalRemained, monthlyInsurance, totalPayment)
    }
  }.toVector


  override def cashflows(maturityMonth: Int): Vector[Cashflow] = {
    (0 to maturityMonth).map {
      case i if i == 0 =>  MortgageReceive(capital)
      case i if i <= maturityMonth =>
        val flow = mortgageTable(i)
        MortgagePayment(flow.totalPayment, flow.interest, flow.insurance, flow.captitalReimbursement)
      case _ => Zero
    }.toVector
  }

  override def value(maturityMonth: Int): Double = {
    if (maturityMonth <= maturityMonth)
      -1 * mortgageTable(maturityMonth).capitalPaid * reimbursementYield
    else
      0.0
  }
}


object Mortgage {
  def computeMensualite(K: Double, t: Double, nbMonths: Int): Double = {
    (K * t / 12.0) / (1 - Math.pow(1 + t / 12.0, -nbMonths))
  }
}