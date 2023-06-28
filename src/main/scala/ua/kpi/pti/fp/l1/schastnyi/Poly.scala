package ua.kpi.pti.fp.l1.schastnyi

object Main {
  def main(args: Array[String]): Unit = {
    val poly = Poly("x", Map(2 -> 1, 0 -> 5))
       

  }
}

case class Poly(
  varName: String,
  coeff: Map[Int, Int],
) {
  def add(other: Poly): Either[String, Poly] = {
    if (varName != other.varName)
      Left("Variable name mismatch")
    else {
      val combinedCoeffs = (coeff.keySet ++ other.coeff.keySet).map { exp =>
        val coeffSum = coeff.getOrElse(exp, 0) + other.coeff.getOrElse(exp, 0)
        exp -> coeffSum
      }.toMap
      Right(Poly(varName, combinedCoeffs))
    }
  }

  def mul(other: Poly): Either[String, Poly] = {
    if (varName != other.varName)
      Left("Variable name mismatch")
    else {
      val multipliedCoeffs = for {
        (exp1, coeff1) <- coeff
        (exp2, coeff2) <- other.coeff
      } yield exp1 + exp2 -> (coeff1 * coeff2)

      Right(Poly(varName, multipliedCoeffs))
    }
  }

  def value(varValue: Int): Int = {
    coeff.foldLeft(0) { case (result, (exp, coeff)) =>
      result + (coeff * math.pow(varValue, exp)).toInt
    }
  }
}

