package ua.kpi.pti.fp.l1.assignment.didukh

import ua.kpi.pti.fp.l1.assignment.Assignment 
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest.L1SimpleTest
import ua.kpi.pti.fp.l1.didukh.{ Var, Num, Bool, Add, And, Cond }

// import ua.kpi.pti.fp.l1.didukh.ExprParse // точно так це треба робтии?

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
    },
    "Add test" -> L1SimpleTest.of {
      val expr = Add(Num(10), Num(20))
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(30)))
    },
    "Add with non-integer values test" -> L1SimpleTest.of {
      val expr = Add(Num(10), Bool(true))
      val result = expr.eval(vars)
      assertEquals(result, Left("Type error: cannot perform addition of non-integer values"))
    },
    "And test" -> L1SimpleTest.of {
      val expr = And(Bool(true), Bool(false))
      val result = expr.eval(vars)
      assertEquals(result, Right(Right(false)))
    },
    "And with non-boolean values test" -> L1SimpleTest.of {
      val expr = And(Bool(true), Num(10))
      val result = expr.eval(vars)
      assertEquals(result, Left("Type error: cannot perform logical AND on non-boolean values"))
    },
    "Cond true test" -> L1SimpleTest.of {
      val expr = Cond(Bool(true), Num(10), Num(20))
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(10)))
    },
    "Cond false test" -> L1SimpleTest.of {
      val expr = Cond(Bool(false), Num(10), Num(20))
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(20)))
    },
    "Cond with non-boolean condition test" -> L1SimpleTest.of {
      val expr = Cond(Num(10), Num(10), Num(20))
      val result = expr.eval(vars)
      assertEquals(result, Left("Type error: condition is not a boolean value"))
    }
  )
}