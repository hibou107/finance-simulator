package com.hibou107.finance

import org.scalatest.{FlatSpec, Matchers}

class SimulatorTests extends FlatSpec with Matchers {
  it should "compute the impot" in {
    Simulator.computeImpot(32000, 1) shouldBe 3893
    Simulator.computeImpot(55950, 3) shouldBe 3714
  }

}
