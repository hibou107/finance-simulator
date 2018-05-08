package com.hibou107.finance

case class Value(value: Double, plusValue: Double)

case class MoneyAccount(currentCapital: Double, interest: Double)


case class TotalAccount(moneyAccount: MoneyAccount, peaAccount: PEA, scpi: SCPI, mortgage: Mortgage, salaryIncome: Double)

object Simulator {

  private def moneyAccountSimulate(originalAccount: MoneyAccount, duration: Int, cashflows: Vector[ComposeCashFlow]): Double = {
    val monthlyRate = Simulator.computeMonthRate(originalAccount.interest)
    cashflows.foldLeft(originalAccount.currentCapital) { case (currentCapital, flow) =>
      currentCapital * (1 + monthlyRate)  + flow.value
    }
  }

  def simulate(originalAccount: TotalAccount, durationMonth: Int): Double = {
    val scpiCashflows = originalAccount.scpi.cashflows(durationMonth)
    val mortgageCashflows = originalAccount.mortgage.cashflows(durationMonth)
    val salaryCashflows = Vector.fill(durationMonth + 1)(SalaryIncome(originalAccount.salaryIncome))
    val composedCashflow = (scpiCashflows, mortgageCashflows, salaryCashflows).zipped.map { case (cf1, cf2, cf3) => ComposeCashFlow(cf1 :: cf2 :: cf3 :: Nil)}

    ???
  }
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
    math.pow(1 + yearRate, 1 / 12.0) - 1
  }


  def main(args: Array[String]): Unit = {
    print(computeImpot(44000, 2.5))
  }
}
