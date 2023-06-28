package ua.kpi.pti.fp.l1.kushnir

class EitherOption[L, A](a: Either[L, Option[A]]) {

  def map[B](f: A => B): EitherOption[L, B] = {
    new EitherOption[L, B] (
      a match {
        case Right(Some(value)) => Right(Some(f(value)))
        case Right(None) => Right(None)
        case Left(error) => Left(error)
      }
    )
  }

  def flatMap[B](f: A => EitherOption[L, B]): EitherOption[L, B] = {
    new EitherOption[L, B](
      a match {
        case Right(Some(value)) => f(value).a
        case Right(None) => Right(None)
        case Left(error) => Left(error)
      }
    )
  }

  def subflatMap[B](f: A => Option[B]): EitherOption[L, B] = {
    new EitherOption[L, B](
      a match {
        case Right(Some(value)) => Right(f(value))
        case Right(None) => Right(None)
        case Left(error) => Left(error)
      }
    )
  }

  def semiFlatMap[B](f: A => Either[L, B]): EitherOption[L, B] = {
    new EitherOption[L, B](
      a match {
        case Right(Some(value)) => f(value) match {
          case Right(result) => Right(Some(result))
          case Left(error) => Left(error)
        }
        case Right(None) => Right(None)
        case Left(error) => Left(error)
      }
    )
  }
}
