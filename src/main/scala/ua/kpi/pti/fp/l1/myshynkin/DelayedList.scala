package ua.kpi.pti.fp.l1.myshynkin
trait DelayedList[A] {
  def toLazyList: LazyList[A]

  def size: Int = toLazyList.size

  def zip[B](ys: DelayedList[B]): DelayedList[(A, B)] = {
    DelayedList.fromLazyList(toLazyList.zip(ys.toLazyList))
  }

  def zipWith[B, C](ys: DelayedList[B], f: (A, B) => C): DelayedList[C] = {
    DelayedList.fromLazyList(toLazyList.zip(ys.toLazyList).map(f.tupled))
  }

  def takeWhile(f: A => Boolean): DelayedList[A] = {
    DelayedList.fromLazyList(toLazyList.takeWhile(f))
  }
}

object DelayedList {
  def fromList[A](xs: List[A]): DelayedList[A] = fromLazyList(xs.to(LazyList))

  def fromLazyList[A](xs: LazyList[A]): DelayedList[A] = new DelayedList[A] {
    override def toLazyList: LazyList[A] = xs
  }
}