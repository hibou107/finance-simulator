package com.hibou107.finance

import org.scalatest.{FlatSpec, Matchers}

class MortgageTests extends FlatSpec with Matchers {
  it should "compute mensualite" in {
    Tools.floor(Mortgage.computeMensualite(300000, 0.02, 25 * 12)) shouldBe 1271.56
  }

  it should "compute tableaux d'amortissements" in {
    val newMortgage = new Mortgage(10000, 20, 0.02, 0.001)
    val flows = newMortgage.cashflows(20)
    flows.head shouldBe MortgageReceive(10000)
    flows.collect { case x: MortgagePayment => x.capitalReimbursement
    }.sum shouldBe (10000.0 +- 0.001)
  }

}
