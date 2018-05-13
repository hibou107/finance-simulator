package com.hibou107.finance

import org.scalatest.{FlatSpec, Matchers}

class SCPITests extends FlatSpec with Matchers {
  it should "generate good cashflow" in {
    val scpi = new SCPI(100, 0.05, 5, 0.01, 0.1)
    val cashflows = scpi.cashflows(10)
    cashflows(5) shouldBe RentalIncome(100 * 0.05 / 12 * (1 - 0.155))
    cashflows(0) shouldBe Payment(-100.0)
    (1 to 4).foreach(i => cashflows(i) shouldBe Zero)
  }

}
