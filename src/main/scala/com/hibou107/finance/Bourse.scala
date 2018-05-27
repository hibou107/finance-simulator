package com.hibou107.finance

class ShareAccount(rawCapital: Double, originalMonth: Int, currentMonth: Int, currentCapital: Double,
                   frais: Double, rendement: Double) {
  private lazy val monthlyRate = Simulator.computeMonthRate(rendement)

  def value(month: Int): Value = {
    val currentValue = currentCapital * Math.pow(1 + monthlyRate, month - currentMonth) * (1 - frais)
    Value(currentValue, currentValue - rawCapital)
  }

  def apport(month: Int, value: Double): ShareAccount = {
    val newValue = currentCapital * Math.pow(1 + monthlyRate, month - currentMonth) + value
    val updatedRawCapital = rawCapital + value
    new ShareAccount(updatedRawCapital, originalMonth, month, newValue, frais, rendement)
  }
}

class PEA(rawCapital: Double, originalMonth: Int, currentMonth: Int, currentCapital: Double, frais: Double, rendement: Double) {
  private lazy val monthlyRate = Simulator.computeMonthRate(rendement)
  val fiscalite = List(0, 2, 5)
  val fisValue = List(0.38, 0.345, 0)
  val diff = (currentMonth - originalMonth) / 12.0

  val tauxImpot = if (diff < 2.0)
    0.38
  else if (diff < 5.0)
    0.345
  else
    0.0

  def value(month: Int): Double = {
    val currentValue = currentCapital * Math.pow(1 + monthlyRate, month - currentMonth) * (1 - frais)
    val plusValue = currentValue - rawCapital
    if (plusValue > 0)
      currentValue * (1 - tauxImpot)
    else
      currentValue
  }

  def apport(month: Int, value: Double): PEA = {
    val newValue = currentCapital * Math.pow(1 + monthlyRate, month - currentMonth) + value
    val updatedRawCapital = rawCapital + value
    new PEA(updatedRawCapital, originalMonth, month, newValue, frais, rendement)
  }
}