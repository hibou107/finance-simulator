package com.hibou107.finance


class Pret(totalMonths: Int, tauxAnnuel: Double, capitalDu: Double, tauxAssurance: Double, tauxRemboursement: Double) {

  case class MonthPret(monthNum: Int, capitalAmorti: Double, interet: Double, capitalDu: Double, assurance: Double, totalPayment: Double)

  private val assuranceMensuel = capitalDu * tauxAssurance / 12.0

  private val tauxMensuel = tauxAnnuel / 12.0

  private val mensualite = Pret.computeMensualite(capitalDu, tauxAnnuel, totalMonths)

  lazy val tableAmortissement = {
    val zero = MonthPret(0, 0, 0, Tools.floor(capitalDu), 0.0, 0.0)
    (1 to totalMonths).scanLeft(zero) { case (lastMonth, currentMonth) =>
      val interet = lastMonth.capitalDu * tauxMensuel
      val capitalRembourse = mensualite - interet
      val capitalAmorti = lastMonth.capitalDu
      val capitalDu = lastMonth.capitalDu - capitalRembourse
      val assurance = assuranceMensuel
      val totalPayment = assurance + mensualite
      MonthPret(currentMonth, capitalAmorti, interet, capitalDu, assurance, totalPayment)
    }
  }.toVector

  def getMonth(monthNum: Int): MonthPret = tableAmortissement(monthNum)

  def toutRembourser(monthNum: Int): Double = getMonth(monthNum).capitalDu * tauxRemboursement

}


object Pret {

  def computeMensualite(K: Double, t: Double, nbMonths: Int) = {
    (K * t / 12.0) / (1 - Math.pow(1 + t / 12.0, -nbMonths))
  }


}