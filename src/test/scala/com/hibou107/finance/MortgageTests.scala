package com.hibou107.finance

import org.scalatest.{FlatSpec, Matchers}

class MortgageTests extends FlatSpec with Matchers {
  it should "compute mensualite" in {
    Tools.floor(Mortgage.computeMensualite(300000, 0.02, 25 * 12)) shouldBe 1271.56
  }

  it should "compute tableaux d'amortissements" in {
    val newMortgage = new Mortgage(10000, 20, 0.02, 0.001, 0.05)
    val flows = newMortgage.cashflows(20)
    flows.head shouldBe MortgageReceive(10000)
    flows.collect { case x: MortgagePayment => x.capitalReimbursement
    }.sum shouldBe (10000.0 +- 0.001)
  }

  it should "give the same result as simulation" in {
    val newMortgage = new Mortgage(135000, 120, 0.016, 0.0012, 0.05)
    (newMortgage.monthlyPayment + newMortgage.monthlyInsurance) shouldBe (1231.65 +- 0.01)

  }

}
