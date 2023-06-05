package ua.kpi.pti.fp.l1.assignment.herashchenko

// import org.scalacheck.{Gen, Prop}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}

import ua.kpi.pti.fp.l1.herashchenko.Monoid
import ua.kpi.pti.fp.l1.herashchenko.Monoid._


case object MonoidTests extends Assignment {
    override def assigneeFullName: String = "Геращенко Володимир Сергійович"
    override val props: List[(String, L1PropOrTest)] = {
        List(
    "list: operation of two non empty elements test" -> L1SimpleTest.of {
        val listMonoid: Monoid[List[Int]] = Monoid[Int,List]
        val list: List[Int] = listMonoid.combine(List(1,2,3), List(4,5,6))
        assert(list.length == 6)
    },
    "set: operation of two non empty elements test" -> L1SimpleTest.of {
        val setMonoid: Monoid[Set[Int]] = Monoid[Int,Set]
        val set: Set[Int] = setMonoid.combine(Set(1,2,3), Set(3,4,5))
        assert(set.equals(Set(1,2,3,4,5)))
    },
    "option: operation of two non empty elements test" -> L1SimpleTest.of {
        val listMonoid: Monoid[Option[Int]] = Monoid[Int,Option]
        val option: Option[Int] = listMonoid.combine(Some(2), Some(1))
        assert(option.get == 2)
    },
    
    )   
    }
    
}
