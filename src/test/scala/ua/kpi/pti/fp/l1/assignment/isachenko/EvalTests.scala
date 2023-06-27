package ua.kpi.pti.fp.l1.assignment.isachenko

import ua.kpi.pti.fp.l1.assignment.L1PropOrTest
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.Assignment

import ua.kpi.pti.fp.l1.isachenko.Eval

object MutualRecursion {
  def even(n: Int): Eval[Boolean] =
    Eval.now(n == 0).flatMap {
      case true => Eval.now(true)
      case false => odd(n - 1)
    }

  def odd(n: Int): Eval[Boolean] =
    Eval.now(n == 0).flatMap {
      case true => Eval.now(false)
      case false => even(n - 1)
    }
}

case object EvalTests extends Assignment {
  override def assigneeFullName: String = "Ісаченко Нікіта Сергійович"
  override val props: List[(String, L1PropOrTest)] = List(
    "Flatmap tests" -> L1SimpleTest.of {
      val res = Eval.now(1).flatMap(x => Eval.later(() => x + 1)).flatMap(x => Eval.later(() => x - 100)).value()

      res match {
        case Left(_) => fail("Exception during evaluation")
        case Right(value) => assert(value == -98)
      }
    },
    "Mutual Recursion computing test" -> L1SimpleTest.of {
      val res = MutualRecursion.even(2000).value()
      res match {
        case Left(_) => fail("Exception during evaluation")
        case Right(value) => assert(value == true)
      }
    },
  )
}
