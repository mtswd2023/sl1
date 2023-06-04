package ua.kpi.pti.fp.l1.didukh

object ExprParse {
  def parse(s: String): Either[String, Expr] = ???
}
// parse("(y & T) ? (a + 1) : 100") == Cond(And(Var('y'), Bool(true)), Add(Var('a'),Num(1)), Num(100))
// use Left(...) to denote errors
