package ua.kpi.pti.fp.l1.lopateckiy

//Option
case class OptFn[A, B](f: A => Option[B]) {
  def map[C](fn: B => C): OptFn[A, C] = OptFn(a => f(a).map(fn))

  def flatMap[C](fn: B => OptFn[A, C]): OptFn[A, C] = OptFn((a: A) =>
    f(a) match {
      case Some(b) => fn(b).f(a)
      case None => None
    },
  )
  def andThen[C](g: B => OptFn[B, C]): OptFn[A, C] = OptFn((a: A) =>
    f(a) match {
      case Some(b) => g(b).f(b)
      case None => None
    },
  )
}
