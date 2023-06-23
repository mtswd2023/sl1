package ua.kpi.pti.fp.l1.assignment.kiyashko

import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.kiyashko.RandomFunctions.{RandomType, double, int,fill, zipWith}
import ua.kpi.pti.fp.l1.kiyashko.{LCGRandom}



case object LcgTests extends Assignment {
  override def assigneeFullName: String = "Кіяшко Ігор Володимирович"

  val seed = 12345

  val random = LCGRandom(seed)

  val fillTest: L1PropOrTest = L1SimpleTest.of {
    val n = 5
    val result = fill(random)(n)
    println(s"result: $result")
  }

  val intTest: L1PropOrTest = L1SimpleTest.of {
    val (value, _) = int(random)
    println(s"int: $value")
  }

  val doubleTest: L1PropOrTest = L1SimpleTest.of {
    val (value, _) = double(random)(random)
    println(s"double: $value")
  }

  val zipWithTest: L1PropOrTest = L1SimpleTest.of {
    val ra: RandomType[Int] = _.next()
    val rb: RandomType[Double] = double(random)
    val f: (Int, Double) => String = (a, b) => s"($a, $b)"
    val (value, _) = zipWith(ra, rb)(f)(random)
    println(s"zipWith: $value")
  }

  override val props: List[(String, L1PropOrTest)] = List(
    "fill test" -> fillTest,
    "int test" -> intTest,
    "double test" -> doubleTest,
    "zipWith test" -> zipWithTest
  )
}