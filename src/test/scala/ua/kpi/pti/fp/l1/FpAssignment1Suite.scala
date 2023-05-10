package ua.kpi.pti.fp.l1

import munit.ScalaCheckSuite
import org.scalacheck.Prop
import ua.kpi.pti.fp.l1.assignment.Assignment
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._


class FpAssignment1Suite extends ScalaCheckSuite {
  Assignment.all.foreach { a =>
    a.props.foreach { case (name, prop) =>
      val testName = s"${a.assigneeFullName} $name"
      prop match {
        case L1Prop(p) =>
          if (p eq Prop.passed) {
            property(testName)(fail("Please don't"))
          } else if (p eq Prop.proved) {
            property(testName)(fail("Please don't"))
          } else if (p eq Prop.undecided) {
            property(testName.ignore)(fail("Please don't"))
          } else {
            property(testName)(p)
          }
        case L1SimpleTest(body) =>
          test(testName)(body())
      }
    }
  }
}
