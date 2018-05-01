package com.hibou107.finance

import scala.util.Try


class Immobilier(prix: Double, rendementImmobilier: Double, apport: Double, tauxNotaire: Double,
                 tauxAgence: Double, nbMonth: Int, tauxAnnuel: Double,
                 tauxAssurance: Double, tauxRemboursement: Double) {

  private val fraisNotaire = tauxNotaire * prix
  private val fraisAgence = tauxAgence * prix
  private val totalPret = prix + fraisAgence + fraisNotaire - apport
  private val pret = new Pret(nbMonth, tauxAnnuel, totalPret, tauxAssurance, tauxRemboursement)
  private val cashflows = (apport * -1 - pret.tableAmortissement.head.totalPayment) +: pret.tableAmortissement.tail.map(_.totalPayment * -1)
  private val rendementMensuel = Simulator.computeMonthRate(rendementImmobilier)

  def cashflow(month: Int): Double = Try(cashflows(month)).toOption.getOrElse(0.0)

  def currentValue(month: Int): Double = {
    val prixImmobilier = prix * math.pow(1 + rendementMensuel, month) * (1 - tauxAgence)
    val capitalDu = Try(pret.tableAmortissement(month)).toOption.fold(0.0)(_.capitalDu)
    val totalRemboursement = capitalDu * (1 + tauxRemboursement)
    prixImmobilier - totalRemboursement
  }

}
