package ua.kpi.pti.fp.l1.assignment.patsora

import org.scalacheck.{Gen, Prop}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.patsora._
import ua.kpi.pti.fp.l1.patsora.Expr._

case object ExprTests extends Assignment {
  override def assigneeFullName: String = "Пацьора Поліна Олегівна"

  override val props: List[(String, L1PropOrTest)] = {
    implicit def arbitrary: Gen[Expr] = Gen.oneOf(
      Gen.choose(0, 100).map(Literal),
      for {
        op <- Gen.oneOf("+", "-", "*", "/")
        left <- arbitrary
        right <- arbitrary
      } yield BinaryOp(op, left, right)
    )

    List(
      "eval(vars) returns Right(Right(value)) for a Literal" -> L1Prop(
        Prop.forAll { (value: Int) =>
          val expr = Literal(value)
          expr.eval(Map.empty) == Right(Right(value))
        }
      ),
      "eval(vars) returns Left(error) for Division by zero" -> L1Prop(
        Prop.forAll(Gen.choose(1, 100), Gen.const(0)) { (left: Int, right: Int) =>
          val expr = BinaryOp("/", Literal(left), Literal(right))
          expr.eval(Map.empty) == Left("Division by zero")
        }
      ),
      "eval(vars) returns Right(Right(result)) for BinaryOp" -> L1Prop(
        Prop.forAll(arbitrary, Gen.choose(1, 100), Gen.choose(1, 100)) { (expr: Expr, left: Int, right: Int) =>
          val vars = Map("left" -> left, "right" -> right)
          expr.eval(vars) match {
            case Right(Right(result)) =>
              val expected = evalBinaryOp(expr.toString, left, right)
              expected == Right(Right(result))
            case _ => false
          }
        }
      ),
      "parse and eval are inverse operations" -> L1Prop(
        Prop.forAll(arbitrary) { (expr: Expr) =>
          val parsedExpr = Expr.parse(expr.toString)
          parsedExpr match {
            case Right(parsed) => parsed.eval(Map.empty) == expr.eval(Map.empty)
            case _ => false
          }
        }
      ),
      "parse fails for invalid expression" -> L1Prop(
        Prop.forAll(Gen.alphaStr) { (invalidExpr: String) =>
          Expr.parse(invalidExpr) == Left("Invalid expression")
        }
      ),
      "parse fails for expression with invalid token" -> L1Prop(
        Prop.forAll(Gen.alphaStr) { (invalidToken: String) =>
          val exprString = s"1 $invalidToken 2"
          Expr.parse(exprString) == Left(s"Invalid token: $invalidToken")
        }
      )
    )
  }
}
