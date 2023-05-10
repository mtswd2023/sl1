package ua.kpi.pti.fp.l1.assignment.tkalenko

import org.scalacheck.{Gen, Prop}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.tkalenko.Nat
import ua.kpi.pti.fp.l1.tkalenko.Nat._

case object NatTests extends Assignment {
  override def assigneeFullName: String = "Ткаленко Роман Юрійович"

  override val props: List[(String, L1PropOrTest)] = {
    implicit val arbitrary: Gen[Nat] = Gen.choose(0, 1_000_000).map(fromIntUnsafe)
    List(
      "fromInt(x).toInt === x" -> L1Prop(
        Prop.forAll(Gen.choose(Int.MinValue, 1_000_000)) { (n: Int) =>
          val maybeInt: Option[Int] = fromInt(n).map(_.toInt())
          (n < 0 && maybeInt.isEmpty) || (n >= 0 && maybeInt.contains(n))
        },
      ),
      "fromInt(n.toInt) === n" -> L1Prop(
        Prop.forAll(arbitrary) { (n: Nat) =>
          fromInt(n.toInt()).contains(n)
        },
      ),
      "simple test" -> L1SimpleTest.of {
        assert(Nat.fromInt(1).contains(Succ(Zero)))
      },
    )
  }

}
