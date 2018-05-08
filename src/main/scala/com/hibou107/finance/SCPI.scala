package com.hibou107.finance

class SCPI(price: Double, rendement: Double, nbMonthJouissance: Int, inflation: Double, sellCost: Double) extends Instrument {
  private val monthlyRental = price * rendement / 12.0

  def cashflows(maturityMonth: Int): Vector[Cashflow] = {
    (0 to maturityMonth).map {
      case 0 => Payment(-price)
      case i if i >= nbMonthJouissance => RentalIncome(monthlyRental)
      case _ => Zero
    }.toVector
  }

  override def value(maturityMonth: Int): Double = price * math.pow(1 + inflation, maturityMonth) * (1 - sellCost)
}


