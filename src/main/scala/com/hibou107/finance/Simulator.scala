package com.hibou107.finance

case class Value(value: Double, plusValue: Double)

sealed trait Impot

case class IR(value: Double) extends Impot

case class PrelevementSocial(value: Double) extends Impot

sealed trait Asset {
  def toMoney(): Double

}


class Simulator(monthlySaving: Double, marketYield: Double) {
  private val monthlyYield = Simulator.computeMonthRate(marketYield)

  def computeMarketInvestment(monthlyInvestment: Double, nbMonth: Int): Value = {
    val first = 0.0
    val lastvalue = (1 to nbMonth).foldLeft(monthlyInvestment) { case (result, _) =>
        result * (1 + monthlyYield) + monthlyInvestment
    }
    val plusValue = lastvalue - monthlyInvestment  * nbMonth
    Value(lastvalue, plusValue)
  }

  def conversionMoney(v: Value, nbMonth: Int): Double = {
    val year = nbMonth / 12.0
    if (year <= 2)
      v.plusValue * (100 - 22.5 - 17.2) / 100.0 + v.value
    else if (year >= 2 && year <= 5 )
      v.plusValue * (100 - 19.0 - 17.2) / 100.0 + v.value
    else
      v.plusValue * (100 - 17.2) / 100.0 + v.value
  }


  def simulate(nbMonth: Int): Vector[Double] = {
    ???
  }
}

object Simulator {

  def computeImpot(netImposable: Double, nbPart: Double): Double = {
    val intervals = List(0, 9807, 27086, 72617, 158783.0)
    val rates = List(0.0, 0.14, 0.3, 0.41, 0.45)
    val byPart = netImposable / nbPart
    val temp = intervals.zip(intervals.tail).zip(rates).foldLeft(0.0) { case (result, ((min, max), rate)) =>
      if (byPart >= max)
        result + (max - min) * rate
      else if (byPart >= min && byPart <= max)
        result + (byPart - min) * rate
      else
        result
    }
    val temp2 = if (byPart >= intervals.last)
      temp + (byPart - intervals.last) * rates.last
    else
      temp
    math.floor(temp2 * nbPart)
  }

  def computeMonthRate(yearRate: Double): Double = {
    math.pow(1 + yearRate, 1 / 12) - 1
  }


  def main(args: Array[String]): Unit = {
    print(computeImpot(44000, 2.5))
  }
}
