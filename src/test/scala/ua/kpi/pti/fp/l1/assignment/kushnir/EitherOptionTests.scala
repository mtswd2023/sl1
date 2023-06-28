package ua.kpi.pti.fp.l1.assignment.kushnir

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

import org.scalacheck.Test
import org.scalacheck.Test.Parameters

import ua.kpi.pti.fp.l1.kushnir.EitherOption

object EitherOptionProperties extends Properties("EitherOption") {

  property("map preserves the structure") = forAll { (eitherOption: EitherOption[String, Int], f: Int => Double) =>
    val mapped = eitherOption.map(f)
    eitherOption.a.isLeft == mapped.a.isLeft && eitherOption.a.forall(_.isDefined) == mapped.a.forall(_.isDefined)
  }

  property("subflatMap preserves the structure") = forAll { (eitherOption: EitherOption[String, Int], f: Int => Option[Double]) =>
    val flatMapped = eitherOption.subflatMap(f)
    eitherOption.a.isLeft == flatMapped.a.isLeft && eitherOption.a.forall(_.isDefined) == flatMapped.a.forall(_.isDefined)
  }

  property("semiFlatMap preserves the structure") = forAll { (eitherOption: EitherOption[String, Int], f: Int => Either[String, Double]) =>
    val flatMapped = eitherOption.semiFlatMap(f)
    eitherOption.a.isLeft == flatMapped.a.isLeft && eitherOption.a.forall(_.isDefined) == flatMapped.a.forall(_.isDefined)
  }

  property("flatMap preserves the structure") = forAll { (eitherOption: EitherOption[String, Int], f: Int => EitherOption[String, Double]) =>
    val flatMapped = eitherOption.flatMap(f)
    eitherOption.a.isLeft == flatMapped.a.isLeft && eitherOption.a.forall(_.isDefined) == flatMapped.a.forall(_.isDefined)
  }
}

object EitherOptionTests extends Assignment{
  val checkConfig = Parameters.default.withMinSuccessfulTests(100)

  val result = Test.check(checkConfig, EitherOptionProperties)

  if (result.passed) {
    println("All tests passed!")
  } else {
    println(s"Some tests failed:\n${result.status}")
  }
  
  override def assigneeFullName: String = "Кушнір Влада Василівна"
}
