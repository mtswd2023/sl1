package ua.kpi.pti.fp.l1.assignment.kiyashko

import ua.kpi.pti.fp.l1.kiyashko.LinearCongruentialGenerator
import org.scalacheck.{Gen, Prop}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}

object LinearCongruentialGeneratorTests {
  def main(args: Array[String]): Unit = {
    val seed = 12345L
    val a = 1103515245L
    val c = 12345L
    val m = math.pow(2, 31).toLong - 1
    val n = 10

    val sequence = LinearCongruentialGenerator.generateSequence(seed, a, c, m, n)
    println(s"Generated sequence: $sequence")

    assert(sequence.length == n, "Sequence length mismatch!")

    assert(sequence.toSet.size == n, "Duplicate elements found in the sequence!")

    val maxValue = sequence.max
    val minValue = sequence.min
    assert(maxValue < m && minValue >= 0, "Generated values are out of range!")

    println("All tests passed!")
  }
}