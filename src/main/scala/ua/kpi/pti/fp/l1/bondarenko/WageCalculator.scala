package ua.kpi.pti.fp.l1.bondarenko

sealed trait Employee
case class Worker(name: String) extends Employee

case class Hour(value: Int)
case class Wage(value: Int)

case class EmployeePayment(employee: Employee, payment: Wage)

object WageCalculator {
  def calculatePayment(hours: Hour, wage: Wage): Either[String, Wage] = {
    if (hours.value <= 10) Right(Wage(hours.value * wage.value))
    else Left("Employee worked for more than 10 hours")
  }

  def hoursToWages(hours: List[Hour], wage: Wage): Either[String, List[Wage]] = {
    val wageCalculations = hours.map(hour => calculatePayment(hour, wage))
    val paymentErrors = wageCalculations.collect { case Left(error) => error }

    if (paymentErrors.nonEmpty) Left(paymentErrors.mkString(", "))
    else Right(wageCalculations.collect { case Right(payment) => payment })
  }

  def hoursToEmployeePayments(hours: List[Hour], employees: List[Employee]): Either[String, List[EmployeePayment]] = {
    if (hours.length != employees.length)
      Left("Number of hours and employees do not match")
    else {
      val wage = Wage(20) // Assuming a fixed wage of 20 units per hour for all employees
      hoursToWages(hours, wage).flatMap { wageList =>
        if (wageList.length != employees.length)
          Left("Number of wage calculations and employees do not match")
        else {
          val employeePayments = wageList.zip(employees).map { case (wage, employee) => EmployeePayment(employee, wage) }
          Right(employeePayments)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val hours = List(Hour(8), Hour(5), Hour(12))
    val employees = List(Worker("John"), Worker("Alice"), Worker("Bob"))

    val result = WageCalculator.hoursToEmployeePayments(hours, employees)
    result match {
      case Left(error) => println(s"Error: $error")
      case Right(payments) => payments.foreach(println)
    }
  }
}
