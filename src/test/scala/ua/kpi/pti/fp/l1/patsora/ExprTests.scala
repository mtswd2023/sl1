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
      "parse and eval are inverse operations" -> L1Prop(
        Prop.forAll { (s: String) =>
          parse(s) match {
            case Left(_) => true
            case Right(expr) => expr.eval(Map.empty) == Right(Right(expr.eval(Map.empty)))
          }
        },
      ),
      "parse fails for invalid expression" -> L1Prop(
        Prop.forAll { (s: String) =>
          parse(s) match {
            case Left(_) => true
            case Right(_) => false
          }
        },
      ),
      "parse fails for expression with invalid token" -> L1Prop(
        Prop.forAll {
          (
            expression: String,
            invalidToken: String,
          ) =>
            val invalidExpression = expression.replace(" ", s" $invalidToken ")
            parse(invalidExpression).isLeft
        },
      ),
    )
  }
}
