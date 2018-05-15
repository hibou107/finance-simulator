package com.hibou107.finance

import scala.annotation.tailrec
import scala.language.postfixOps

object InternalRateOfReturn {

  case class CashFlow(value: Double, annualFraction: Double)

  def computeInternalRateOfReturn(cashFlow: List[CashFlow]): Option[Double] = {
    def f(rate: Double): Double = {
      cashFlow map { case CashFlow(value, annualFraction) =>
        value / Math.pow(1 + rate, annualFraction)
      } sum
    }
    RootFinder.findRootSecant(f, -0.1, 0.1, 0.0001, 0, 1000)
  }

  def main(args: Array[String]): Unit = {

  }


}