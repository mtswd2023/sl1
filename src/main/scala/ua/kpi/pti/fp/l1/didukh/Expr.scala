package ua.kpi.pti.fp.l1.didukh

sealed trait Expr {
  def typecheck(): Option[String]

  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]]
}

case class Var(name: String) extends Expr {
  def typecheck(): Option[String] = None

  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]] = {
    vars.get(name) match {
      case Some(value) => Right(value)
      case None => Left(s"Variable $name is undefined")
    }
  }
}

case class Num(value: Int) extends Expr {
  def typecheck(): Option[String] = None

  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]] =
    Right(Left(value))
}

case class Bool(value: Boolean) extends Expr {
  def typecheck(): Option[String] = None

  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]] =
    Right(Right(value))
}

case class Add(
  expr1: Expr,
  expr2: Expr,
) extends Expr {
  def typecheck(): Option[String] = {
    (expr1.typecheck(), expr2.typecheck()) match {
      case (None, None) => None
      case (Some(error), None) => Some(error)
      case (None, Some(error)) => Some(error)
      case (Some(error1), Some(error2)) => Some(s"Type error: $error1 and $error2")
    }
  }

  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]] = {
    (expr1.eval(vars), expr2.eval(vars)) match {
      case (Right(Left(value1)), Right(Left(value2))) => Right(Left(value1 + value2))
      case _ => Left("Type error: cannot perform addition of non-integer values")
    }
  }
}

case class And(
  expr1: Expr,
  expr2: Expr,
) extends Expr {
  def typecheck(): Option[String] = {
    (expr1.typecheck(), expr2.typecheck()) match {
      case (None, None) => None
      case (Some(error), None) => Some(error)
      case (None, Some(error)) => Some(error)
      case (Some(error1), Some(error2)) => Some(s"Type error: $error1 and $error2")
    }
  }

  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]] = {
    (expr1.eval(vars), expr2.eval(vars)) match {
      case (Right(Right(value1)), Right(Right(value2))) => Right(Right(value1 && value2))
      case _ => Left("Type error: cannot perform logical AND on non-boolean values")
    }
  }
}

case class Cond(
  bool: Expr,
  expr1: Expr,
  expr2: Expr,
) extends Expr {
  def typecheck(): Option[String] = {
    (bool.typecheck(), expr1.typecheck(), expr2.typecheck()) match {
      case (None, None, None) => None
      case (Some(error), _, _) => Some(error)
      case (_, Some(error), _) => Some(error)
      case (_, _, Some(error)) => Some(error)
    }
  }

  def eval(vars: Map[String, Either[Int, Boolean]]): Either[String, Either[Int, Boolean]] = {
    bool.eval(vars) match {
      case Right(Right(true)) => expr1.eval(vars)
      case Right(Right(false)) => expr2.eval(vars)
      case _ => Left("Type error: condition is not a boolean value")
    }
  }
}
