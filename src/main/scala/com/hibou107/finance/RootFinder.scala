package com.hibou107.finance

import scala.annotation.tailrec

object RootFinder {
  @tailrec
  def findRootBisection(f: Double => Double, min: Double, max: Double, error: Double): Option[Double] = {
    val mid = (max + min) / 2
    if (Math.abs(max - min) < error)
      Some(mid)
    else {
      val fMax = f(max)
      val fMin = f(min)
      val fMid = f(mid)
      if ((fMax * fMid) <= 0.0)
        findRootBisection(f, mid, max, error)
      else if ((fMin * fMid) <= 0.0)
        findRootBisection(f, min, mid, error)
      else
        None
    }
  }


  @tailrec
  def findRootSecant(f: Double => Double, x0: Double, x1: Double, error: Double, current: Int, max: Int): Option[Double] = {
    if (current == max)
      None
    else if (x1 == x0)
      Some(x0 )
    else if (math.abs((x0 - x1) / x0) < error)
      Some(x0)
    else {
      val x2 = x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))
      findRootSecant(f, x1, x2, error, current + 1, max)
    }
  }




}
