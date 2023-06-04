package ua.kpi.pti.fp.l1.herashchenko


trait Monoid[A] {
    def combine(
        x: A,
        y: A,
    ): A
    def empty: A
}
object Monoid {

    implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]]{
        override def empty: List[A] = List.empty[A]
        override def combine(x: List[A], y: List[A]): List[A] = x ++ y
    }
    implicit def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
        override def empty: Option[A] = None
        override def combine(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
    }
    implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]]{
        override def empty: Set[A] = Set.empty[A]
        override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
    }
}


trait Empty[F[_]]{
    def get[A]: F[A]
}
trait Combine[F[_]]{
    def combine[A](x:F[A], y:F[A]): F[A]
}
object Empty{   
    implicit val listEmpty: Empty[List] = new Empty[List]{
        def get[A]: List[A] = List.empty[A]
    }
    implicit val optionEmpty: Empty[Option] = new Empty[Option]{
        def get[A]: Option[A] = None
    }
    implicit val setEmpty: Empty[Set] = new Empty[Set]{
        def get[A]: Set[A] = Set.empty[A]
    }
}
object Combine{
    implicit val listCombine: Combine[List] = new Combine[List]{
        def combine[A](x: List[A], y: List[A]): List[A] = x ++ y
    }
    implicit val optionCombine: Combine[Option] = new Combine[Option]{
        def combine[A](x: Option[A], y: Option[A]): Option[A] = x orElse y
    }
    implicit val setCombine: Combine[Set] = new Combine[Set]{
        def combine[A](x: Set[A], y: Set[A]): Set[A] = x ++ y
    }
}
