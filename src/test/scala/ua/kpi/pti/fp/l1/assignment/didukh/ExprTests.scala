package ua.kpi.pti.fp.l1.assignment.didukh

import ua.kpi.pti.fp.l1.assignment.Assignment 
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest.L1SimpleTest
// import ua.kpi.pti.fp.l1.didukh.Expr

object ExprTests extends Assignment {
  override def assigneeFullName: String = "Дідух Максим Андрійович"

  override def props: List[(String, L1PropOrTest)] = List(
    "1 + 1 == 2" -> L1SimpleTest.of(assert(1+1 == 2))
  )

}