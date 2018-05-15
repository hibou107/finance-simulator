package com.hibou107.finance

import com.hibou107.finance.InternalRateOfReturn.CashFlow
import org.scalatest.{FlatSpec, Matchers}

class InternalRateOfReturnTests extends FlatSpec with Matchers {
  it should "find root Bisection" in {
    val result = RootFinder.findRootBisection(x => x + 1, -2, 3, 0.01).get
    result shouldBe >= (-1 - 0.01)
    result shouldBe <= (-1 + 0.01)
  }

  it should "find root secant" in {
    val result = RootFinder.findRootSecant(x => x + 1, -2, 3, 0.01, 0, 100).get
    result shouldBe >= (-1 - 0.01)
    result shouldBe <= (-1 + 0.01)
  }

  it should "compute internal rate of return" in {
    val result = InternalRateOfReturn.computeInternalRateOfReturn(List(CashFlow(-123400, 0), CashFlow(36200, 1), CashFlow(54800, 2), CashFlow(48100, 3)))
    result shouldBe >= (0.0596 - 0.0001)
    result shouldBe <= (0.0596 + 0.0001)
  }


}
