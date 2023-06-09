package ua.kpi.pti.fp.l1.tkalenko

import scala.annotation.tailrec

sealed trait Nat {
  @tailrec
  private def toIntAcc(
    n: Nat,
    curr: Int,
  ): Int = {
    n match {
      case _: Nat.Zero.type => curr
      case s: Nat.Succ => toIntAcc(s.n, curr + 1)
    }
  }

  def toInt(): Int = {
    toIntAcc(this, 0)
  }

  def +(that: Nat): Nat = {
    @tailrec
    def rec(
      curr: Nat,
      sum: Nat,
    ): Nat = {
      curr match {
        case Nat.Zero => sum
        case Nat.Succ(n) => rec(n, Nat.Succ(sum))
      }
    }

    rec(this, that)
  }

  // Some(this - that) if this >= that
  // None otherwise
  def -(that: Nat): Option[Nat] = {
    import Nat._
    @tailrec
    def rec(
      a: Nat,
      b: Nat,
    ): Option[Nat] = {
      (a, b) match {
        case (_, Zero) => Some(a)
        case (Zero, _) => None
        case (Succ(pa), Succ(pb)) => rec(pa, pb)
      }
    }

    rec(this, that)
  }

  override def toString(): String = s"${toInt()}"

  override def hashCode(): Int = toInt().hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case n: Nat => toInt() == n.toInt()
    case _ => false
  }
}

object Nat {

  case object Zero extends Nat

  case class Succ(n: Nat) extends Nat

  @tailrec
  private def fromIntAcc(
    i: Int,
    curr: Nat,
  ): Option[Nat] = {
    if (i < 0) {
      None
    } else if (i == 0) {
      Some(curr)
    } else {
      fromIntAcc(i - 1, Nat.Succ(curr))
    }
  }

  def fromInt(i: Int): Option[Nat] = {
    fromIntAcc(i, Zero)
  }

  def fromIntUnsafe(i: Int): Nat = {
    require(i >= 0)

    @tailrec
    def fromIntAcc(
      i: Int,
      curr: Nat,
    ): Nat = {
      if (i == 0) {
        curr
      } else {
        fromIntAcc(i - 1, Nat.Succ(curr))
      }
    }

    fromIntAcc(i, Zero)
  }
}
