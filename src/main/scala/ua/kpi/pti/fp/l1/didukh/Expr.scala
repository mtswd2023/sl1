package ua.kpi.pti.fp.l1.didukh

sealed trait Expr {
  // Var(name) | Num(int) | Bool(boolean) | Add(expr, expr) | And(expr, expr) | Cond(bool, expr, expr
  // Some(error) if we have e.g. Add(Bool(true), Num(10)), None otherwise
  def typecheck(): Option[String]
  // use left to denote errors, right for results
  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]]
  // note that this is incomplete - please refine type hierarchy in a way that could
  // prevent e.g. having Add(Bool,Num) or And(Num,Num). Maybe we can have different sets of types
  // for results of parsing and actual Expr?
}
