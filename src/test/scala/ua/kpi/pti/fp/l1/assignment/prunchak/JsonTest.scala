package ua.kpi.pti.fp.l1.assignment.prunchak

import org.scalacheck.Gen
import org.scalacheck.Prop
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.prunchak.{Num, Str, Null, Obj, Arr, JsonStart}

case object JsonTest extends Assignment {
  override def assigneeFullName: String = "Прунчак Кирило Миколайович"

  override val props: List[(String, L1PropOrTest)] = {
    lazy val arbitrary: Gen[JsonStart] = Gen.oneOf(
      Gen.choose(0.0, 1000.0).map(Num),
      Gen.alphaStr.map(Str),
      Gen.const(Null: JsonStart),
      Gen.listOf(arbitrary).map(Arr),
      Gen.mapOf(Gen.zip(Gen.alphaStr, arbitrary)).map(Obj),
    )

    List(
      "Num is correctly converted to toString" -> L1Prop(
        Prop.forAll(arbitrary) { json: JsonStart =>
          json match {
            case Num(n) => json.toString == n.toString
            case _ => true
          }
        },
      ),
      "Str is correctly converted to toString" -> L1Prop(
        Prop.forAll(arbitrary) { json: JsonStart =>
          json match {
            case Str(s) => json.toString == s""""$s""""
            case _ => true
          }
        },
      ),
      "Null is correctly converted to toString" -> L1Prop(
        Prop.forAll(arbitrary) { json: JsonStart =>
          json match {
            case Null => json.toString == "null"
            case _ => true
          }
        },
      ),
      "Arr is correctly converted to toString" -> L1Prop(
        Prop.forAll(arbitrary) { json: JsonStart =>
          json match {
            case Arr(xs) =>
              val expected = xs.map(_.toString).mkString("[", ",", "]")
              json.toString == expected
            case _ => true
          }
        },
      ),
      "Obj is correctly converted to toString" -> L1Prop(
        Prop.forAll(arbitrary) { json: JsonStart =>
          json match {
            case Obj(vs) =>
              val properties = vs.map { case (key, value) => s""""$key":${value.toString}""" }
              val expected = properties.mkString("{", ",", "}")
              json.toString == expected
            case _ => true
          }
        },
      ),
      "findFirstValue returns the correct value for Obj" -> L1Prop(
        Prop.forAll(arbitrary) { json: JsonStart =>
          json match {
            case obj: Obj =>
              val key = "testKey"
              val value = Num(42)
              val testObj = Obj(obj.vs + (key -> value))
              val result = testObj.findFirstValue(key)
              result.contains(value)
            case _ => true
          }
        },
      ),
    )
  }
}
