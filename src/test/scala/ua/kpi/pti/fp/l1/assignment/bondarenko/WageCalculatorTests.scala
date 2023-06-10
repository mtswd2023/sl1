package ua.kpi.pti.fp.l1.assignment.bondarenko

import ua.kpi.pti.fp.l1.assignment.{Assignment}
import ua.kpi.pti.fp.l1.bondarenko._
import org.junit.Test
case object WageCalculatorTests extends Assignment{
  override def assigneeFullName: String = "Бондаренко Олександр Сергійович"


  class WageCalculatorTest {

    @Test
    def testCalculatePayment(): Unit = {
      val result1 = WageCalculator.calculatePayment(Hour(8), Wage(10))
      assert(result1 == Right(Wage(80)), "calculatePayment test 1 failed")

      val result2 = WageCalculator.calculatePayment(Hour(12), Wage(15))
      assert(result2 == Left("Employee worked for more than 10 hours"), "calculatePayment test 2 failed")

    }

    @Test
    def testCalculatePaymentExceedingLimit(): Unit = {
      val hours = List(Hour(8), Hour(5), Hour(12))
      val wage = Wage(20)

      val result = WageCalculator.calculatePayment(hours.last, wage)

      result match {
        case Right(_) => fail("Expected Left, got Right")
        case Left(error) => assertEquals("Employee worked for more than 10 hours", error)
      }
    }


    @Test
    def  testHoursToWagesWithEmptyHours(): Unit = {
      val hours = List.empty[Hour]
      val wage = Wage(10)

      val result9 = WageCalculator.hoursToWages(hours, wage)
      assert(result9 == Right(List.empty[Wage]), "hoursToWages test 3 failed")
    }

    @Test
    def testHoursToEmployeePaymentsWithEmptyHoursAndEmployee(): Unit = {
      val hours = List.empty[Hour]
      val employees = List.empty[Worker]

      val result10 = WageCalculator.hoursToEmployeePayments(hours, employees)
      assert(result10 == Right(List.empty[EmployeePayment]), "hoursToEmployeePayments test 3 failed")
    }

  }

}
