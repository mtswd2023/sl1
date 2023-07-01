package ua.kpi.pti.fp.l1.assignment.myshynkin
import ua.kpi.pti.fp.l1.myshynkin.DelayedList
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest.L1SimpleTest
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}

case object DelayedListTest extends Assignment {
  override def assigneeFullName: String = "Мишинкін Богдан Сергійович"

  val testList = DelayedList.fromList(List(1, 2, 3, 4, 5))

  override def props: List[(String, L1PropOrTest)] = List(
    "Size test" -> L1SimpleTest.of {
      val result = testList.size
      assertEquals(result, 5) 
    },
    "Zip test" -> L1SimpleTest.of {
      val zipped = testList.zip(DelayedList.fromList(List(6, 7, 8, 9, 10)))
      val result = zipped.toLazyList
      assertEquals(result, LazyList((1, 6), (2, 7), (3, 8), (4, 9), (5, 10))) 
    },
    "ZipWith test" -> L1SimpleTest.of {
      val zipWithSum = testList.zipWith(DelayedList.fromList(List(6, 7, 8, 9, 10)), (a: Int, b: Int) => a + b)
      val result = zipWithSum.toLazyList
      assertEquals(result, LazyList(7, 9, 11, 13, 15))
    },
    "TakeWhile test" -> L1SimpleTest.of {
      val taken = testList.takeWhile(_ < 4)
      val result = taken.toLazyList
      assertEquals(result, LazyList(1, 2, 3))
    },
    "fromList test" -> L1SimpleTest.of {
      val fromListTest = DelayedList.fromList(List(10, 20, 30))
      val result = fromListTest.toLazyList
      assertEquals(result, LazyList(10, 20, 30))
    },
    "fromLazyList test" -> L1SimpleTest.of {
      val fromLazyListTest = DelayedList.fromLazyList(LazyList(40, 50, 60))
      val result = fromLazyListTest.toLazyList
      assertEquals(result, LazyList(40, 50, 60))
    }
  )
}