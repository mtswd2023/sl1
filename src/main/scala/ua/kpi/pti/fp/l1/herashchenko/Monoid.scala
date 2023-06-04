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

