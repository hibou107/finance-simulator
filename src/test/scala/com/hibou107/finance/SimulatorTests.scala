package com.hibou107.finance

import com.hibou107.finance.InternalRateOfReturn.CashFlow
import org.scalatest.{FlatSpec, Matchers}

class SimulatorTests extends FlatSpec with Matchers {
  it should "compute the taxes" in {
    Simulator.computeIR(32000, 1) shouldBe 3893
    Simulator.computeIR(55950, 3) shouldBe 3714
    Simulator.computeIR(58500, 2.5) shouldBe 4757
  }

  it should "simulate correctly" in {
    val scpi = new SCPI(150000, 0.06, 5, 0.005, 0.1)
    val mortgage = new Mortgage(135000, 10 * 12, 0.016, 0.0012, 0.05)
    val allCashflows = Simulator.simulate(scpi, mortgage, 5200, 10 * 12)
    val cf = allCashflows.zipWithIndex.map { case (flow, index) =>
      CashFlow(flow.value, index / 12)
    }.toList
    println(InternalRateOfReturn.computeInternalRateOfReturn(cf))
  }
}
