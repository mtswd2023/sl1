package ua.kpi.pti.fp.l1.kiyashko

object LinearCongruentialGenerator {

  def generateSequence(seed: Long, a: Long, c: Long, m: Long, n: Int): List[Long] = {
    var current = seed
    var result = List[Long]()

    for (_ <- 1 to n) {
      current = (a * current + c) % m
      result = current :: result
    }

    result.reverse
  }
}