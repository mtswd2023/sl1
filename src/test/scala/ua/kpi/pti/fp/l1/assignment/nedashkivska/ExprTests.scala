package ua.kpi.pti.fp.l1.assignment.nedashkivska

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
// import org.scalacheck.Arbitrary
// import org.scalacheck.Arbitrary.arbitrary
// import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.nedashkivska._

case object ExprTests extends Assignment {

  override def assigneeFullName: String = "Недашківська Аріна Віталіївна"

  implicit val arbitrary: Gen[Double] = Gen.choose(0, 1_000_000.0)

  val subProp: Prop = forAll {
    (
      d1: Double,
      d2: Double,
    ) =>
      val expr1: Num = Num(d1)
      val expr2: Num = Num(d2)
      val subExpr: Sub = Sub(expr1, expr2)
      subExpr.value() == (d1 - d2)
  }

  val mulProp: Prop = forAll {
    (
      d1: Double,
      d2: Double,
    ) =>
      val expr1: Num = Num(d1)
      val expr2: Num = Num(d2)
      val mulExpr: Mul = Mul(expr1, expr2)
      mulExpr.value() == (d1 * d2)
  }

  val divProp: Prop = forAll {
    (
      d1: Double,
      d2: Double,
    ) =>
      if (d2 != 0) {
        val expr1: Num = Num(d1)
        val expr2: Num = Num(d2)
        val divExpr: Div = Div(expr1, expr2)
        divExpr.value() == (d1 / d2)
      } else {
        true
      }
  }

  override val props: List[(String, L1PropOrTest)] = List(
    "Num should return the correct value" -> L1Prop(Prop.forAll(arbitrary) { (d: Double) =>
      val num: Num = Num(d)
      num.value() == d
    }),
    "Add must correctly calculate the sum of two expressions" -> L1Prop(Prop.forAll(arbitrary, arbitrary) {
      (
        d1: Double,
        d2: Double,
      ) =>
        val expr1: Num = Num(d1)
        val expr2: Num = Num(d2)
        val addExpr: Add = Add(expr1, expr2)
        addExpr.value() == (d1 + d2)
    }),
    "Sub must correctly calculate the difference between two expressions" -> L1Prop(Prop.forAll(arbitrary, arbitrary) {
      (
        d1: Double,
        d2: Double,
      ) =>
        val expr1: Num = Num(d1)
        val expr2: Num = Num(d2)
        val subExpr: Sub = Sub(expr1, expr2)
        subExpr.value() == (d1 - d2)
    }),
    "Mul must correctly calculate the product of two expressions" -> L1Prop(Prop.forAll(arbitrary, arbitrary) {
      (
        d1: Double,
        d2: Double,
      ) =>
        val expr1: Num = Num(d1)
        val expr2: Num = Num(d2)
        val mulExpr: Mul = Mul(expr1, expr2)
        mulExpr.value() == (d1 * d2)
    }),
    "Div must correctly calculate the quotient of two expressions" -> L1Prop(Prop.forAll(arbitrary, arbitrary) {
      (
        d1: Double,
        d2: Double,
      ) =>
        val expr1: Num = Num(d1)
        val expr2: Num = Num(d2)
        val divExpr: Div = Div(expr1, expr2)
        divExpr.value() == (d1 / d2)
    }),
  )

}
