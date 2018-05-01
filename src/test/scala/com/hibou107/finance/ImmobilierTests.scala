package com.hibou107.finance

import org.scalatest.{FlatSpec, Matchers}

class ImmobilierTests extends FlatSpec with Matchers {
  it should "compute value immobilier" in {
    val immobilier = new Immobilier(300000, 0.03, 30000, 0.08,
      0.05, 240, 0.02, 0.001, 0.05)
    (0 to 500).foreach { i =>
      println(i, immobilier.cashflow(i), immobilier.currentValue(i))
    }
  }
}
