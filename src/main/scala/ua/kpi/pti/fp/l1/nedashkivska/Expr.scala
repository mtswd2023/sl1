package ua.kpi.pti.fp.l1.nedashkivska

trait Expr {
  def value(): Double
}

case class Num(n: Double) extends Expr {
  override def value(): Double = n
}

case class Add(
  expr1: Expr,
  expr2: Expr,
) extends Expr {
  override def value(): Double = expr1.value() + expr2.value()
}

case class Sub(
  expr1: Expr,
  expr2: Expr,
) extends Expr {
  override def value(): Double = expr1.value() - expr2.value()
}

case class Mul(
  expr1: Expr,
  expr2: Expr,
) extends Expr {
  override def value(): Double = expr1.value() * expr2.value()
}

case class Div(
  expr1: Expr,
  expr2: Expr,
) extends Expr {
  override def value(): Double = expr1.value() / expr2.value()
}
