package ua.kpi.pti.fp.l1.patsora

sealed trait Expr {
  def eval(vars: Map[String, Int]): Either[String, Either[Expr, Int]]
}

case class BinaryOp(op: String, left: Expr, right: Expr) extends Expr {
  override def eval(vars: Map[String, Int]): Either[String, Either[Expr, Int]] = {
    left.eval(vars) match {
      case Left(error) => Left(error)
      case Right(Left(updatedLeft)) =>
        Right(Left(BinaryOp(op, updatedLeft, right)))
      case Right(Right(leftValue)) =>
        right.eval(vars) match {
          case Left(error) => Left(error)
          case Right(Left(updatedRight)) =>
            Right(Left(BinaryOp(op, Literal(leftValue), updatedRight)))
          case Right(Right(rightValue)) =>
            Expr.evalBinaryOp(op, leftValue, rightValue)
        }
    }
  }
}

case class Literal(value: Int) extends Expr {
  override def eval(vars: Map[String, Int]): Either[String, Either[Expr, Int]] =
    Right(Right(value))
}

object Expr {
  private val operators = Set("+", "-", "*", "/")

  def parse(s: String): Either[String, Expr] = {
    def parseExpr(tokens: List[String]): Either[String, (Expr, List[String])] = tokens match {
      case Nil => Left("Unexpected end of expression")
      case ")" :: _ => Left("Mismatched parentheses")
      case token :: remaining =>
        if (operators.contains(token)) {
          parseExpr(remaining) match {
            case Left(error) => Left(error)
            case Right((leftExpr, rest)) =>
              parseExpr(rest) match {
                case Left(error) => Left(error)
                case Right((rightExpr, remainingTokens)) =>
                  Right((BinaryOp(token, leftExpr, rightExpr), remainingTokens))
              }
          }
        } else if (token == "(") {
          parseExpr(remaining) match {
            case Left(error) => Left(error)
            case Right((expr, ")" :: rest)) => Right((expr, rest))
            case _ => Left("Mismatched parentheses")
          }
        } else {
          try {
            Right((Literal(token.toInt), remaining))
          } catch {
            case _: NumberFormatException => Left(s"Invalid token: $token")
          }
        }
    }

    val tokens = s.split("\\s+").toList
    parseExpr(tokens) match {
      case Left(error) => Left(error)
      case Right((expr, Nil)) => Right(expr)
      case _ => Left("Invalid expression")
    }
  }

  def evalBinaryOp(op: String, left: Int, right: Int): Either[String, Either[Expr, Int]] = {
    op match {
      case "+" => Right(Right(left + right))
      case "-" => Right(Right(left - right))
      case "*" => Right(Right(left * right))
      case "/" =>
        if (right != 0) Right(Right(left / right))
        else Left("Division by zero")
      case _ => Left(s"Unknown operator: $op")
    }
  }
}
