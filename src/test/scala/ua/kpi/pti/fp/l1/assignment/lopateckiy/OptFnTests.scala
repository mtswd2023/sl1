package ua.kpi.pti.fp.l1.assignment.lopateckiy

import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
import ua.kpi.pti.fp.l1.lopateckiy.OptFn

case object OptFnTests extends Assignment {
  override def assigneeFullName: String = "Лопатецький Михайло Володимирович"
  override val props: List[(String, L1PropOrTest)] = {
    List(
      "map: function from int to Option and then +1 (returns Some for positive)" -> L1SimpleTest.of {
        val optFn = OptFn((a: Int) => if (a > 0) Some(a * 2) else None)
        val mappedFn = optFn.map(_ + 1)

        assert(mappedFn.f(3) == (Some(7)))
      },
      "map: function from Int to Option and then +1 (returns None for negative)" -> L1SimpleTest.of {
        val optFn = OptFn((a: Int) => if (a > 0) Some(a * 2) else None)
        val mappedFn = optFn.map(_ + 1)

        assert(mappedFn.f(-2) == (None))
      },
    "flatMap: function from Int to Boolean (if Int returns None)" -> L1SimpleTest.of {
      val fn1: Int => Option[String] = (_: Int) => None
      val fn2: String => OptFn[Int, Boolean] = (s: String) => OptFn((x: Int) => Some(s.length > x))

      val opt1 = OptFn(fn1)
      val opt2 = opt1.flatMap(fn2)

      assert(opt2.f(0) == (None))
    },

      "flatMap: function from Int to Boolean (if Int returns Some)" -> L1SimpleTest.of {
        val fn1: Int => Option[String] = (x: Int) => if (x > 0) Some((x * 2).toString) else None
        val fn2: String => OptFn[Int, Boolean] = (s: String) => OptFn((x: Int) => Some(s.length > x))

        val opt1 = OptFn(fn1)
        val opt2 = opt1.flatMap(fn2)

        assert(opt2.f(-2) == (None))
      },
      "flatMap: function from Int to Boolean (if Int returns Some)" -> L1SimpleTest.of {
        val fn1: Int => Option[String] = (x: Int) => if (x > 0) Some((x * 2).toString) else None
        val fn2: String => OptFn[Int, Boolean] = (s: String) => OptFn((x: Int) => Some(s.length > x))

        val opt1 = OptFn(fn1)
        val opt2 = opt1.flatMap(fn2)

        assert(opt2.f(3) == (Some(false)))
      },
    )
  }
}
