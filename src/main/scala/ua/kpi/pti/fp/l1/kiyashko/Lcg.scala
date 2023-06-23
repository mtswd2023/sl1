package ua.kpi.pti.fp.l1.kiyashko

trait Random {
  def next(): (Int, Random)
}

case class LCGRandom(seed: Int) extends Random {
  private val a: Int = 1664525
  private val c: Int = 1013904223
  private val m: Int = math.pow(2, 32).toInt

  override def next(): (Int, Random) = {
    val nextSeed = (seed * a + c) % m
    (nextSeed, LCGRandom(nextSeed))
  }
}

object RandomFunctions {
  type RandomType[A] = Random => (A, Random)

  def fill(r: Random)(n: Int): List[Int] = {
    if (n <= 0) {
      Nil
    } else {
      val (nextValue, nextRandom) = r.next()
      nextValue :: fill(nextRandom)(n - 1)
    }
  }

  val int: RandomType[Int] = _.next()

  def double(r: Random): RandomType[Double] = { _ =>
    val (value, nextRandom) = r.next()
    (value.toDouble / Int.MaxValue, nextRandom)
  }

  def zipWith[A, B, C](
    ra: RandomType[A],
    rb: RandomType[B]
    )(
    f: (A, B) => C
    ): RandomType[C] = { rng =>
    val (valueA, nextRandomA) = ra(rng)
    val (valueB, nextRandomB) = rb(nextRandomA)
    (f(valueA, valueB), nextRandomB)
  }
}

