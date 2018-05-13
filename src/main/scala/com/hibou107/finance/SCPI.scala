package com.hibou107.finance


class SCPI(price: Double, thisYield: Double, nbMonthJouissance: Int, inflation: Double, sellCost: Double) extends Instrument {
  private val monthlyRental = price * thisYield / 12.0 * (1 - 0.155)
  val monthlyInflation: Double = Simulator.computeMonthRate(inflation)

  def cashflows(maturityMonth: Int): Vector[UnitCashflow] = {
    (0 to maturityMonth).map {
      case 0 => Payment(-price)
      case i if i >= nbMonthJouissance => RentalIncome(monthlyRental)
      case _ => Zero
    }.toVector
  }

  def cashflowsSellAt(sellAt: Int, maturityMonth: Int): Vector[UnitCashflow] = {
    val cf = cashflows(maturityMonth)
    cf.zipWithIndex.map {
      case (_, index) if index == cf.size - 1 => SCPISell(value(index + 1), price)
      case (_, index) if index > cf.size - 1 => Zero
      case (v, _) => v
    }
  }

  override def value(maturityMonth: Int): Double = price * math.pow(1 + monthlyInflation, maturityMonth) * (1 - sellCost)
}

