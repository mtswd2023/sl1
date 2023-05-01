package ua.kpi.pti.fp.l1

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import ua.kpi.pti.fp.l1.Nat._

class NatTest extends ScalaCheckSuite {
  property("fromInt->toInt roundtrip") {}
}
