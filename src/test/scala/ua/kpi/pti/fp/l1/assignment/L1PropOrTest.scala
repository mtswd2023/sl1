package ua.kpi.pti.fp.l1.assignment

import org.scalacheck.Prop

sealed trait L1PropOrTest
object L1PropOrTest {
  case class L1Prop(p: Prop) extends L1PropOrTest
  case class L1SimpleTest(body: () => Unit) extends L1PropOrTest
  object L1SimpleTest {
    def of(body: => Unit): L1SimpleTest = new L1SimpleTest(() => body)
  }
}
