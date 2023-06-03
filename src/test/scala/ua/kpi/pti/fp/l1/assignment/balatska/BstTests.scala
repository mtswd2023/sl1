package ua.kpi.pti.fp.l1.assignment.balatska

import org.scalacheck.{Prop}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}

case object BstTests extends Assignment {
  override def assigneeFullName: String = "Балацька Вікторія Віталіївна"
  override def props: List[(String, L1PropOrTest)] = List("default" -> L1Prop(Prop.undecided))
}
