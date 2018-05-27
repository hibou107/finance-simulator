package com.hibou107.finance

import scala.language.postfixOps

case class Value(value: Double, plusValue: Double)

object Simulator {


  def addIRCashflowsByProject(composedCashflow: Vector[MultiCashFlow], nbPart: Double): Vector[MultiCashFlow] = {
    val yearlyFlows = composedCashflow.grouped(12).map { cashflowOneYear =>
      val salariesIncome = cashflowOneYear.flatMap(_.values.collect { case SalaryIncome(v) => v}) sum
      val salaryTaxable = salariesIncome * 0.9
      val rentPayment = cashflowOneYear.flatMap(_.values.collect { case RentalIncome(v) => v}) sum
      val interestPayment = cashflowOneYear.flatMap(_.values.collect { case MortgagePayment(_, interest, _, _) => interest}) sum
      val netImposable = salaryTaxable + rentPayment - interestPayment
      Simulator.computeIR(salaryTaxable, nbPart) - Simulator.computeIR(netImposable, nbPart)
    } toVector

    composedCashflow.zipWithIndex.map {
      case (flow, index) if (index % 12 == 0) && (index > 0) => MultiCashFlow(IRPayment(yearlyFlows(index / 12 - 1)) :: flow.values)
      case (flow, _) => flow
    }
  }


  def simulate(scpi: SCPI, mortgage: Mortgage, monthlySalary: Double, durationMonth: Int): Vector[MultiCashFlow] = {
    val scpiCashflows = scpi.cashflowsSellAt(durationMonth, durationMonth)

    val mortgageCashflows = mortgage.cashflows(durationMonth)

    val salaryCashflows = Vector.fill(durationMonth + 1)(SalaryIncome(monthlySalary))

    val composedCashflow = (scpiCashflows, mortgageCashflows, salaryCashflows).zipped.map {
      case (cf1, cf2, cf3) => MultiCashFlow(cf1 :: cf2 :: cf3 :: Nil)
    }

    val withIRCashflows = addIRCashflowsByProject(composedCashflow, 2.5)
    withIRCashflows.map {
      case MultiCashFlow(values) => MultiCashFlow(values.filter {
        case _: SalaryIncome => false
        case _ => true
      })
    }
  }


  def computeIR(netImposable: Double, nbPart: Double): Double = {
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
    print(computeIR(44000+1500*12, 2.5))
  }
}
