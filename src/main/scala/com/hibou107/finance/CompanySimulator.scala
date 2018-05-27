package com.hibou107.finance

object CompanySimulator {

  def compute(sales: Double, expense: Double, myWifeAnnualSalary: Double) = {
    val benefit = sales - expense
    val remuneration = benefit / 1.47
    println(remuneration + myWifeAnnualSalary)
    val ir = Simulator.computeIR((remuneration + myWifeAnnualSalary) * 0.9, 2.5)
    remuneration + myWifeAnnualSalary - ir
}

def main(args: Array[String]): Unit = {
  (0 to 20) foreach { i =>
    val sale = 50000 + i * 5000
    val revenu = compute(sale, 2000, 1500*12)
    println(s"$sale,$revenu")
  }
}
}
