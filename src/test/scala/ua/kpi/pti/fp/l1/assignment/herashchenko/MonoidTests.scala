package ua.kpi.pti.fp.l1.assignment.herashchenko

import org.scalacheck.Prop

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary 


import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.Assignment

import ua.kpi.pti.fp.l1.assignment.L1PropOrTest
import ua.kpi.pti.fp.l1.herashchenko.Monoid
import ua.kpi.pti.fp.l1.herashchenko.Monoid._



case object MonoidTests extends Assignment {
    override def assigneeFullName: String = "Геращенко Володимир Сергійович"

    val listIntGenerator: Gen[List[Int]] = Gen.listOf(arbitrary[Int])
    implicit val listIntArbitrary: Arbitrary[List[Int]] = Arbitrary(listIntGenerator)

    val setIntGenerator: Gen[Set[Int]] = Gen.containerOf[Set, Int](arbitrary[Int])
    implicit val setIntArbitrary: Arbitrary[Set[Int]] = Arbitrary(setIntGenerator)

    val optionIntGenerator: Gen[Option[Int]] = Gen.option(arbitrary[Int])
    implicit val optionIntArbitrary: Arbitrary[Option[Int]] = Arbitrary(optionIntGenerator)
        


    def neutral[A](monoid: Monoid[A])(implicit arb: Arbitrary[A]):Prop = forAll{(a:A) => (monoid.combine(monoid.empty, a) ==  a) && (monoid.combine(a, monoid.empty) == a)}

    def assoc[A](monoid: Monoid[A])(implicit arb: Arbitrary[A]):Prop = forAll{(a1: A, a2: A, a3: A) => 
        monoid.combine(monoid.combine(a1,a2), a3) == monoid.combine(a1,monoid.combine(a2,a3))}


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
    
    "list: associative axiom property" -> L1Prop{
        val listMonoid: Monoid[List[Int]] = Monoid[Int,List]
        assoc[List[Int]](listMonoid)
    },
    "list: neutral axioms property" -> L1Prop{
        val listMonoid: Monoid[List[Int]] = Monoid[Int,List]
        neutral[List[Int]](listMonoid)
    },
    "set: associative axiom property" -> L1Prop{
        val setMonoid: Monoid[Set[Int]] = Monoid[Int,Set]
        assoc[Set[Int]](setMonoid)
    },
    "set: neutral axioms property" -> L1Prop{
        val setMonoid: Monoid[Set[Int]] = Monoid[Int,Set]
        neutral[Set[Int]](setMonoid)
    },
    "option: associative axiom property" -> L1Prop{
        val optionMonoid: Monoid[Option[Int]] = Monoid[Int,Option]
        assoc[Option[Int]](optionMonoid)
    },
    "option: neutral axioms property" -> L1Prop{
        val optionMonoid: Monoid[Option[Int]] = Monoid[Int,Option]
        neutral[Option[Int]](optionMonoid)
    },       
    )   
    }
}
