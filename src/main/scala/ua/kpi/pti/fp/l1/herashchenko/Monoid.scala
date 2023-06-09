package ua.kpi.pti.fp.l1.herashchenko

trait Monoid[A] {
  def combine(
    x: A,
    y: A,
  ): A
  def empty: A
}
trait Empty[F[_]] {
  def get[A]: F[A]
}
trait Combine[F[_]] {
  def combine[A](
    x: F[A],
    y: F[A],
  ): F[A]
}

object Monoid {

  def apply[A, F[_]](
    implicit
    monoid: Monoid[F[A]],
  ): Monoid[F[A]] = monoid

  implicit def genericMonoid[A, F[_]](
    implicit
    empt: Empty[F],
    comb: Combine[F],
  ) = new Monoid[F[A]] {
    override def combine(
      x: F[A],
      y: F[A],
    ): F[A] = comb.combine(x, y)
    override def empty: F[A] = empt.get
  }
}

object Empty {
  def apply[F[_]](
    implicit
    instance: Empty[F],
  ): Empty[F] = instance

  implicit val listEmpty: Empty[List] = new Empty[List] {
    def get[A]: List[A] = List.empty[A]
  }
  implicit val optionEmpty: Empty[Option] = new Empty[Option] {
    def get[A]: Option[A] = None
  }
  implicit val setEmpty: Empty[Set] = new Empty[Set] {
    def get[A]: Set[A] = Set.empty[A]
  }
}
object Combine {
  def apply[F[_]](
    implicit
    instance: Combine[F],
  ): Combine[F] = instance

  implicit val listCombine: Combine[List] = new Combine[List] {
    def combine[A](
      x: List[A],
      y: List[A],
    ): List[A] = x ++ y
  }
  implicit val optionCombine: Combine[Option] = new Combine[Option] {
    def combine[A](
      x: Option[A],
      y: Option[A],
    ): Option[A] = x orElse y
  }
  implicit val setCombine: Combine[Set] = new Combine[Set] {
    def combine[A](
      x: Set[A],
      y: Set[A],
    ): Set[A] = x ++ y
  }
}
