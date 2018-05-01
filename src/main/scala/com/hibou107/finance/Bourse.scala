package com.hibou107.finance

class CompteTitre(capital: Double, frais: Double, rendement: Double) {

  def value(): Double = {
    val monthlyRate = Simulator.computeMonthRate(rendement)
    capital * monthlyRate
  }
}


class Bourse {

}
