package com.hibou107.finance

import scala.annotation.tailrec

object InternalRateOfReturn {

  case class CashFlow(value: Double, annualFraction: Double)

  @tailrec
  def findRoot(f: Double => Double, min: Double, max: Double, error: Double): Option[Double] = {
    val mid = (max + min) / 2
    if (Math.abs(max - min) < error)
      Some(mid)
    else {
      val fMax = f(max)
      val fMin = f(min)
      val fMid = f(mid)
      if ((fMax * fMid) <= 0.0)
        findRoot(f, mid, max, error)
      else if ((fMin * fMid) <= 0.0)
        findRoot(f, min, mid, error)
      else
        None
    }
  }

  def computeInternalRateOfReturn(cashFlow: List[CashFlow]): Option[Double] = {
    def f(rate: Double): Double = {
      cashFlow map { case CashFlow(value, annualFraction) =>
        value / Math.pow(1 + rate, annualFraction)
      } sum
    }
    findRoot(f, -5, 5, 0.0001)
  }

  def main(args: Array[String]): Unit = {

  }


}