package ua.kpi.pti.fp.l1.prunchak

trait JsonStart {
  override def toString: String = {
    this match {
      case Num(n) => s"$n"
      case Str(string) =>
        val modifiedString = string
          .replace(""""""", """\"""")
          .replace("""\\""", """\""")
          .replace("""\\\""", """\""")
          .replace("""//""", """/""")
          .replace("""///""", """/""")
        modifiedString
      case Null => "null"
      case Arr(xs) => xs.map(_.toString).mkString("[", ",", "]")
      case Obj(vs) =>
        val properties = vs.map { case (key, value) => s""""$key":${value.toString}""" }
        properties.mkString("{", ",", "}")
      case _ => ""
    }
  }

  def findFirstValue(key: String): Option[JsonStart] = {
    this match {
      case Obj(vs) =>
        vs.get(key) match {
          case Some(value) => Some(value)
          case None =>
            vs.values.flatMap(_.findFirstValue(key)).headOption
        }
      case Arr(xs) =>
        xs.flatMap(_.findFirstValue(key)).headOption
      case _ => None
    }
  }

}

case class Num(n: Double) extends JsonStart

case class Str(string: String) extends JsonStart

case object Null extends JsonStart

case class Arr(xs: Seq[JsonStart]) extends JsonStart

case class Obj(vs: Map[String, JsonStart]) extends JsonStart
