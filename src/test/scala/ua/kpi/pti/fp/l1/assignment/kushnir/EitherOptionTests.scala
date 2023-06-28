package ua.kpi.pti.fp.l1.assignment.kushnir

import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.kushnir.EitherOption

case object EitherOptioTests extends Assignment {
    override val props: List[(String, L1PropOrTest)]={
        List(
            "map should apply the function to the wrapped value" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(Some(5)))
                val result = EitherOption(Right(Some(10)))
                assertResult(result) {
                    eitherOption.map(_ * 2)
                }
            }

            "return None when the wrapped value is None" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(None))
                val result = EitherOption(Right(None))
                assertResult(result) {
                    eitherOption.map((a: Int) => a * 2)
                }
            }
            "propagate the left value when the Either is a Left" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Left("Error"))
                val result = EitherOption(Left("Error"))
                assertResult(result) {
                    eitherOption.map((a: Int) => a * 2)
                }
            }
            "flatMap should apply the function to the wrapped value and flatten the result" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(Some(5)))
                val result = eitherOption.flatMap((a: Int) => EitherOption(Right(Some(a * 2))))
                val b = Eitheroption(Right(Some(10)))
                assert (result == b)
            }
            "return None when the wrapped value is None" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(None))
                val result = eitherOption.flatMap((a: Int) => EitherOption(Right(Some(a * 2))))
                val b = Eitheroption(Right(None))
                assert (result == b)
            }
            "propagate the left value when the Either is a Left" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Left("Error"))
                val result = eitherOption.flatMap((a: Int) => EitherOption(Right(Some(a * 2))))
                val b = Eitheroption(Left("Error"))
                assert (result == b)
            }
            "subflatMap should apply the function to the wrapped value and return the transformed Option" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(Some(5)))
                val result = eitherOption.subflatMap((a: Int) => Some(a * 2))
                val b = Eitheroption(Right(Some(10)))
                assert (result == b)
            }
            "return None when the wrapped value is None" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(None))
                val result = eitherOption.subflatMap((a: Int) => Some(a * 2))
                val b = Eitheroption(Right(None))
                assert (result == b)
            }
            "propagate the left value when the Either is a Left" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Left("Error"))
                val result = eitherOption.subflatMap((a: Int) => Some(a * 2))
                val b = Eitheroption(Left("Error"))
                assert (result == b)
            }
            "semiFlatMap should apply the function to the wrapped value and return the resulting EitherOption" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(Some(5)))
                val result = eitherOption.semiFlatMap((a: Int) => Right(a * 2))
                val b = Eitheroption(Right(Some(10)))
                assert (result == b)
            }
            "return None when the wrapped value is None" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Right(None))
                val result = eitherOption.semiFlatMap((a: Int) => Right(a * 2))
                val b = Eitheroption(Right(None))
                assert (result == b)
            }
            "propagate the left value when the Either is a Left" -> L1SimpleTest.of {
                val eitherOption = EitherOption(Left("Error"))
                val result = eitherOption.semiFlatMap((a: Int) => Right(a * 2))
                val b = Eitheroption(Left("Error"))
                assert (result == b)
            }
        )
    }
      override def assigneeFullName: String = "Кушнір Влада Василівна"
    }