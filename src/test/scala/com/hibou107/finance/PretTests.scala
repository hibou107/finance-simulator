package com.hibou107.finance

import org.scalatest.{FlatSpec, Matchers}

class PretTests extends FlatSpec with Matchers {
  it should "compute mensualite" in {
    Tools.floor(Pret.computeMensualite(300000, 0.02, 25 * 12)) shouldBe 1271.56
  }

  it should "compute tableaux d'amortissements" in {
    val pret = new Pret(20, 0.02, 10000, 0.001)
    pret.createTableAmortissements().foreach(println)
  }

}
