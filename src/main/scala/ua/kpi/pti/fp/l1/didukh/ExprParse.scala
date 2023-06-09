package ua.kpi.pti.fp.l1.didukh

class Parser {
  private var pos: Int = 0
  private var input: String = ""

  def parse(s: String): Either[String, Expr] = {
    pos = 0
    input = s.replaceAll("\\s", "")
    parseExpr() match {
      case Right(expr) =>
        if (pos == input.length) Right(expr)
        else Right(expr)
      case Left(error) => Left(error)
    }
  }

  private def parseExpr(): Either[String, Expr] = {
    parseCond() match {
      case Right(expr) => Right(expr)
      case Left(error) => Left(error)
    }
  }

  private def parseCond(): Either[String, Expr] = {
    parseBool() match {
      case Right(boolExpr) =>
        if (pos < input.length && input(pos) == '?') {
          pos += 1
          parseExpr() match {
            case Right(expr1) =>
              if (pos < input.length && input(pos) == ':') {
                pos += 1
                parseExpr() match {
                  case Right(expr2) => Right(Cond(boolExpr, expr1, expr2))
                  case Left(error) => Left(error)
                }
              } else {
                Left("Expected ':'")
              }
            case Left(error) => Left(error)
          }
        } else {
          Right(boolExpr)
        }
      case Left(error) => Left(error)
    }
  }

  private def parseBool(): Either[String, Expr] = {
    parseAnd() match {
      case Right(expr) => Right(expr)
      case Left(error) => Left(error)
    }
  }

  private def parseAnd(): Either[String, Expr] = {
    parseAdd() match {
      case Right(expr1) =>
        if (pos < input.length && input(pos) == '&') {
          pos += 1
          parseAnd() match {
            case Right(expr2) => Right(And(expr1, expr2))
            case Left(error) => Left(error)
          }
        } else {
          Right(expr1)
        }
      case Left(error) => Left(error)
    }
  }

  private def parseAdd(): Either[String, Expr] = {
    parseTerm() match {
      case Right(expr1) =>
        if (pos < input.length && input(pos) == '+') {
          pos += 1
          parseAdd() match {
            case Right(expr2) => Right(Add(expr1, expr2))
            case Left(error) => Left(error)
          }
        } else {
          Right(expr1)
        }
      case Left(error) => Left(error)
    }
  }

  private def parseTerm(): Either[String, Expr] = {
    if (pos < input.length) {
      val c = input(pos)
      pos += 1
      c match {
        case '(' =>
          parseExpr() match {
            case Right(expr) =>
              if (pos < input.length && input(pos) == ')') {
                pos += 1
                Right(expr)
              } else {
                Left("Expected ')'")
              }
            case Left(error) => Left(error)
          }
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          var numStr = c.toString
          while (pos < input.length && input(pos).isDigit) {
            numStr += input(pos)
            pos += 1
          }
          Right(Num(numStr.toInt))
        case 'T' | 'F' =>
          Right(Bool(c == 'T'))
        case _ =>
          if (c.isLetter) {
            Right(Var(c.toString))
          } else {
            Left(s"Invalid character: $c")
          }
      }
    } else {
      Left("Unexpected end of input")
    }
  }
}
