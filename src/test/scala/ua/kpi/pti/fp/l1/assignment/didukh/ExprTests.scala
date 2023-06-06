package ua.kpi.pti.fp.l1.assignment.didukh

import ua.kpi.pti.fp.l1.assignment.Assignment 
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest.L1SimpleTest
import ua.kpi.pti.fp.l1.didukh.{ Var, Num, Bool }
// import ua.kpi.pti.fp.l1.didukh.Add
// import ua.kpi.pti.fp.l1.didukh.Cond
// import ua.kpi.pti.fp.l1.didukh.And


// import ua.kpi.pti.fp.l1.didukh.ExprParse

object ExprTests extends Assignment {
  override def assigneeFullName: String = "Дідух Максим Андрійович"

  val vars = Map("x" -> Left(10), "y" -> Right(true))

  override def props: List[(String, L1PropOrTest)] = List(
    "Var test" -> L1SimpleTest.of {
      val expr = Var("x")
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(10)))
    },
    "Num test" -> L1SimpleTest.of {
      val expr = Num(42)
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(42)))
    },
    "Bool test" -> L1SimpleTest.of {
      val expr = Bool(true)
      val result = expr.eval(vars)
      assertEquals(result, Right(Right(true)))
    }
  )
}