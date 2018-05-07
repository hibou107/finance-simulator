package com.hibou107.finance

class SCPI(price: Double, rendement: Double, nbMonthJouissance: Int) extends Instrument {
  private val monthlyRental = price * rendement / 12.0
  def cashflows(maturityMonth: Int): Vector[Cashflow] = {
    (0 to maturityMonth).map {
      case 0 => Payment(-price)
      case i if i >= nbMonthJouissance => RentalIncome(monthlyRental)
      case _ => Zero
    }.toVector
  }
}


