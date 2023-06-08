package ua.kpi.pti.fp.l1.assignment.didukh

import ua.kpi.pti.fp.l1.assignment.Assignment 
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest.L1SimpleTest
import ua.kpi.pti.fp.l1.didukh.{ Var, Num, Bool, Add, And, Cond, Parser }


object ExprTests extends Assignment {
  override def assigneeFullName: String = "Дідух Максим Андрійович"

  val vars = Map("x" -> Left(10), "y" -> Right(true), "q" -> Left(-666), "p" -> Left(666))
  val parser = new Parser()
  override def props: List[(String, L1PropOrTest)] = List(
    "Var test" -> L1SimpleTest.of {
      val expr = Var("x")
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(10)))
    },
    "Var with boolean variable test" -> L1SimpleTest.of {
      val expr = Var("y")
      val result = expr.eval(vars)
      assertEquals(result, Right(Right(true)))
    },
    "Num test" -> L1SimpleTest.of {
      val expr = Num(42)
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(42)))
    },
    "Num with zero value test" -> L1SimpleTest.of {
      val expr = Num(0)
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(0)))
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
    "Add with variables" -> L1SimpleTest.of {
      val expr = Add(Var("q"), Var("p"))
      val result = expr.eval(vars)
      assertEquals(result, Right(Left(0)))
    },
    "And test" -> L1SimpleTest.of {
      val expr = And(Bool(true), Bool(false))
      val result = expr.eval(vars)
      assertEquals(result, Right(Right(false)))
    },
    "And with true values test" -> L1SimpleTest.of {
      val expr = And(Bool(true), Bool(true))
      val result = expr.eval(vars)
      assertEquals(result, Right(Right(true)))
    },
    "And with false values test" -> L1SimpleTest.of {
      val expr = And(Bool(false), Bool(false))
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
    },
    "Parsing expression from example test" -> L1SimpleTest.of {
      val input = "(y & T) ? (a + 1) : 100"
      val expected = Cond(And(Var("y"), Bool(true)), Add(Var("a"), Num(1)), Num(100))
      val result = parser.parse(input)
      assertEquals(result, Right(expected))
    },
    "Parsing sum of two Vars" -> L1SimpleTest.of {
      val input = "x + y"
      val expected = Add(Var("x"), Var("y"))
      val result = parser.parse(input)
      assertEquals(result, Right(expected))
    },
    "Parsing sum of two Nums test" -> L1SimpleTest.of {
      val input = "(10 + 23)"
      val expected = Add(Num(10), Num(23))
      val result = parser.parse(input)
      assertEquals(result, Right(expected))
    },
    "Evaluation sum of two Nums test" -> L1SimpleTest.of {
      val input = "10 + 23"
      val expected = Right(Left(33))
      val expr = parser.parse(input)
      val result = expr.flatMap(_.eval(vars))
      assertEquals(result, expected)
    },
    "Evaluation modified expression from example test" -> L1SimpleTest.of {
      val input = "(y & T) ? (x + 1) : 100"
      val expected = Right(Left(11))
      val expr = parser.parse(input)
      val result = expr.flatMap(_.eval(vars))
      assertEquals(result, expected)
    }
  )
}
