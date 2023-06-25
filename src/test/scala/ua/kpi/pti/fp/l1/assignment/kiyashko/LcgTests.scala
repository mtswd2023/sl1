package ua.kpi.pti.fp.l1.assignment.kiyashko

import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.kiyashko.{LCGRandom}
import ua.kpi.pti.fp.l1.kiyashko.RandomFunctions



case object LcgTests extends Assignment {
  override def assigneeFullName: String = "Кіяшко Ігор Володимирович"

  val seed = 12345

  val random = LCGRandom(seed)

  override val props: List[(String, L1PropOrTest)] = {
    List(
      "fill test" -> L1SimpleTest.of {
        val rng2 = LCGRandom(456)
        val filledList = RandomFunctions.fill(rng2)(5)
        assert(filledList == List(346244101, -1116184729, 1542165675, -1688264335, 1796741411))
      },
      "int test" -> L1SimpleTest.of {
        val rng3 = LCGRandom(789)
        val (intValue, _) = RandomFunctions.int(rng3)
        assert(intValue == 2007237766)
      },
      "double test" -> L1SimpleTest.of {
        val rng4 = LCGRandom(987)
        val (doubleValue, _) = RandomFunctions.double(rng4)(random)
        assert(doubleValue == 0.40563762981614447)
      },
      "zipWith test" -> L1SimpleTest.of {
        val rng5 = LCGRandom(654)
        val (zippedValue, _) = RandomFunctions.zipWith(RandomFunctions.int, RandomFunctions.double(random))(_ + _)(rng5)
        assert(zippedValue == 2007237766.4056376)
      },
    )
  }
}
