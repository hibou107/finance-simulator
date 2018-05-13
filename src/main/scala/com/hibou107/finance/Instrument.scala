package com.hibou107.finance

trait Instrument {
  def cashflows(maturityMonth: Int): Vector[UnitCashflow]
  def value(maturityMonth: Int): Double
}
