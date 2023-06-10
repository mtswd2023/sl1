package ua.kpi.pti.fp.l1.prunchak

import scala.util.chaining.scalaUtilChainingOps

sealed trait Json {
  override def toString: String = {
    this match {
      case Num(n) => s"$n"
      case Str(string) =>
        string
          .replace(""""""", """\"""")
          .replace("""\\""", """\""")
          .replace("""\\\""", """\""")
          .replace("""//""", """/""")
          .replace("""///""", """/""")
          .pipe(s => s""""$s"""")
      case Null => "null"
      case Arr(xs) => xs.map(s => s"""$s""").mkString("[", ",", "]")
      case Obj(vs) =>
        val properties = vs.map { case (key, value) => s""""$key":${value.toString}""" }
        properties.mkString("{", ",", "}")
    }
  }

  def findFirstValue(key: String): Option[Json] = {
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

case class Num(n: Double) extends Json

case class Str(string: String) extends Json

case object Null extends Json

case class Arr(xs: Seq[Json]) extends Json

case class Obj(vs: Map[String, Json]) extends Json
