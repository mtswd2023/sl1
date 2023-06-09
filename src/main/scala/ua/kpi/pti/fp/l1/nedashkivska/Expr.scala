package ua.kpi.pti.fp.l1.nedashkivska

trait Expr {
  def value(): Double
}

case class Num(n: Double) extends Expr {
  override def value(): Double = n
}

case class Add(expr1: Expr, expr2: Expr) extends Expr {
  override def value(): Double = expr1.value() + expr2.value()
}

case class Sub(expr1: Expr, expr2: Expr) extends Expr {
  override def value(): Double = expr1.value() - expr2.value()
}

case class Mul(expr1: Expr, expr2: Expr) extends Expr {
  override def value(): Double = expr1.value() * expr2.value()
}

case class Div(expr1: Expr, expr2: Expr) extends Expr {
  override def value(): Double = expr1.value() / expr2.value()
}

object Result {
  def main(args: Array[String]): Unit = {

    val expr1 = Num(10)
    val expr2 = Add(expr1, Num(2))
    val expr3 = Sub(expr1, Num(3))
    val expr4 = Mul(expr1, Num(4))
    val expr5 = Div(expr1, Num(5))

    println(s"Expression 1 value: ${expr1.value()}")
    println(s"Expression 2 value: ${expr2.value()}")
    println(s"Expression 3 value: ${expr3.value()}")
    println(s"Expression 4 value: ${expr4.value()}")
    println(s"Expression 5 value: ${expr5.value()}")

    val combinedExpr = Add(expr2, Mul(expr3, Div(expr4, expr5)))

    val result = combinedExpr.value()

    println(s"Результат обчислення: $result")
  }
}