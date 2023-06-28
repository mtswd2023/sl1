package ua.kpi.pti.fp.l1.kushnir

class EitherOption[L, A](a: Either[L, Option[A]]) {

  def map[B](f: A => B): EitherOption[L, B] = {
    new EitherOption[L, B](a.map(_.map(f)))
  }

  def flatMap[B](f: A => EitherOption[L, B]): EitherOption[L, B] = {
    new EitherOption[L, B](a.flatMap {
      case Some(value) => f(value).a
      case None => Right(None)
    })
  }

  def subflatMap[B](f: A => Option[B]): EitherOption[L, B] = {
    new EitherOption[L, B](a.map(_.flatMap(f)))
  }

  def semiFlatMap[B](f: A => Either[L, B]): EitherOption[L, B] = {
    new EitherOption[L, B](a.flatMap{
      case Some(value) => f(value).map(Some(_))
      case None => Right(None)
    })
  }
}
