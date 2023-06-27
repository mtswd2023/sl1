package ua.kpi.pti.fp.l1.isachenko

import ua.kpi.pti.fp.l1.isachenko.Eval._

sealed trait Eval[A] {
  def value(): Either[Throwable, A]
  def flatMap[B](f: A => Eval[B]): Eval[B] = this match {
    case _: FlatMap[_, _] => new FlatMap(this, f)
    case e: Now[_] => new FlatMap(e, f)
    case e: Later[_] => new FlatMap(e, f)
  }
}

object Eval {

  private case class Now[A](a: A) extends Eval[A] {
    override def value(): Either[Throwable, A] = Right(a)
  }

  private class Later[A](f: () => A) extends Eval[A] {
    private var evaluation: () => A = f

    override def value(): Either[Throwable, A] =
      try {
        val result = evaluation()
        evaluation = null
        Right(result)
      } catch {
        case e: Throwable => Left(e)
      }
  }

  private class FlatMap[A, B](
    c: Eval[A],
    f: A => Eval[B],
  ) extends Eval[B] {
    override def value(): Either[Throwable, B] = {
      val comp = c.value()
      comp match {
        case Left(value) => Left(value)
        case Right(value) => f(value).value()
      }
    }
  }

  def now[A](a: A): Eval[A] = Now(a)
  def later[A](a: () => A): Eval[A] = new Later(a)
}
