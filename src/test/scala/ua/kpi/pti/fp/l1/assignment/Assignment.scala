package ua.kpi.pti.fp.l1.assignment

import org.scalacheck.Prop
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.tkalenko.NatTests
import ua.kpi.pti.fp.l1.assignment.patsora
import ua.kpi.pti.fp.l1.assignment.nedashkivska
import ua.kpi.pti.fp.l1.assignment.Doroshenko.TreeTests
import ua.kpi.pti.fp.l1.assignment.lopateckiy.OptFnTests
import ua.kpi.pti.fp.l1.assignment.herashchenko.MonoidTests
import scala.annotation.unused
import ua.kpi.pti.fp.l1.assignment.balatska.BstTests
import ua.kpi.pti.fp.l1.assignment.prunchak.JsonTest
import ua.kpi.pti.fp.l1.assignment.myshynkin.DelayedListTest
// Please put your definitions into separate packages upon implementation
// All implementations should come up with a set of reasonable laws
// Feel free to add any number of helper functions, convenient constructors etc etc
// Also feel free to change type hierarchies, fields etc as long as the behavior is
//  consistent with the requirements
// Please also remove @unused annotation from your code - it's there at the moment
//  since most of the code is... well, actually unused so when you implement everything,
//  there should be no need to have it.
// Data structures should be immutable, functions preferably tail-recursive.
//  It's OK to use mutability locally (i.e. in a way that doesn't escape boundaries
//  of a single function and is not observed externally). BUT only use mutable data or
//  algorithms thar rely on mutation if you can prove that there is no easy/efficient way
//  to do it while staying in the immutability land.
// Please write as many properties as you can. If you can't come up with a law,
// provide a simple test instead.
trait Assignment extends munit.Assertions {
  def assigneeFullName: String
  def props: List[(String, L1PropOrTest)] = List("default" -> L1Prop(Prop.undecided))
}

object Assignment {
  val all: List[Assignment] = List(
    NatTests,
    TreeTests,
    OptFnTests,
    MonoidTests,
    JsonTest,
    DelayedListTest,
    new Assignment {
      override def assigneeFullName: String = "Гриценко Марія Дмитрівна"
      // implement
      // class Tree[A] = Leaf(a:A)|Branch(l:Tree[A],r:Tree[A])
      // sealed?
      // tail-recursive methods
      @unused trait Tree[A] {
        def map[B](f: A => B): Tree[B]
        def size(): Int
        def find(f: A => Boolean): Option[A]
        def toList(): List[A]
        def limitToDepth(n: Int): Tree[A] // cuts branches deeper than n
      }
    },
    new Assignment {
      override def assigneeFullName: String = "Кіяшко Ігор Володимирович"
      // https://en.wikipedia.org/wiki/Linear_congruential_generator
      // state is not stored, it's returned as the _2 of the next() call
      trait Random {
        def next(): (Int, Random)
      }
      type RandomType[A] = Random => (A, Random)
      @unused def fill(r: Random)(n: Int): List[Int] = ??? // list of n random ints
      @unused val int: RandomType[Int] = _.next()
      @unused def double(r: Random): RandomType[Double] = ???
      @unused def zipWith[A, B, C](
        ra: RandomType[A],
        rb: RandomType[B],
      )(
        f: (A, B) => C,
      ): RandomType[C] = ???
    },
    new Assignment {
      override def assigneeFullName: String = "Корнійчук Іван Геннадійович"
      trait DelayedList[A] {
        // Empty | DelayedCons(head: A, tail: () => DelayedList[A])

        @unused def size(): Int
        @unused def map[B](f: A => B): DelayedList[B]
        @unused def filter(f: A => Boolean): DelayedList[A]
        @unused def flatMap[B](f: A => DelayedList[B]): DelayedList[B]
      }
      // fills list with Bs lazily until next()._2 is not None
      @unused def unfold[A, B](zero: A)(next: A => (A, Option[B])): DelayedList[B] = ???
    },
    new Assignment {
      override def assigneeFullName: String = "Куц Дмитро Сергійович"

      trait DelayedList[A] {
        // Empty | DelayedCons(head: A, tail: () => DelayedList[A])
        @unused def toList(): List[A]
        @unused def foldRight[B](z: => B)(f: (A => B) => B): B
        @unused def map[B](f: A => B): DelayedList[B] // in terms of foldRight
      }
      // fills list with As lazily until next()._2 is not None
      @unused def iterate[A](zero: A)(next: A => (A, Option[A])): DelayedList[A] = ???
    },
    new Assignment {
      override def assigneeFullName: String = "Кушнір Влада Василівна"
      // note how we can do
      // Option(1).flatMap(a => Option(a + 2).filter(_ % 2 == 0)).flatMap(x => Some(x + 10)) // == None
      // and
      // Right(1).flatMap(a => Right(a + 2).filterOrElse(_ % 2 == 0, "Not an even number")).flatMap(x => Right(x < 123)) // Left(not an even number)
      // but we can't do
      // Option(1).flatMap(a => Right(a + 1)).flatMap(x => if (x > 10) Some(x + 1) else Some(x - 1))
      // write a class that would allow nesting Option and Either
      // and define its own methods like flatMapOption/flatMapEither/flatMap (with corresponding signatures)
      // so that we can use Option with Either transparently
      @unused
      class EitherOption[L, A](@unused a: Either[L, Option[A]]) {
        @unused def map[B](f: A => B): EitherOption[L, B] = ???
        @unused def flatMap[B](f: A => EitherOption[L, B]): EitherOption[L, B] = ???
        @unused def subflatMap[B](f: A => Option[B]): EitherOption[L, B] = ???
        @unused def semiFlatMap[B](f: A => Either[L, B]): EitherOption[L, B] = ???
      }
    },
new Assignment {
      override def assigneeFullName: String = "Мишинкін Богдан Сергійович"
      trait DelayedList[A] {
        // Empty | DelayedCons(head: A, tail: () => DelayedList[A])
        @unused def size(): Int
        @unused def zip[B](ys: DelayedList[B]): DelayedList[(A, B)]
        @unused def zipWith[B, C](
          ys: DelayedList[B],
          f: (A, B) => C,
        ): DelayedList[C]
        @unused def takeWhile(f: A => Boolean): DelayedList[A]
      }
      // fills list with Bs lazily until next()._2 is not None
      @unused def fromList[A](xs: List[A]): DelayedList[A] = ???
    },
    nedashkivska.ExprTests,
    new Assignment {
      override def assigneeFullName: String = "Прунчак Кирило Миколайович"
      @unused trait Json {
        // Num(n: Double) | Str(string) | Null /*object*/ | Arr(xs: Json) | Obj(vs: Map[String, Json])
        @unused def toString(): String
        // findFirstValue(Obj(Map(a->Num(1),b->Arr(Num(12),Num(32),Obj(x->Str("asd"))), "x") == "asd"
        @unused def findFirstValue(key: String): Json
        // insertAtFirst(Obj(Map(a->Num(1),b->Arr(Num(12),Num(32),Obj(x->Str("asd"))), "x", Num(123)) ==
        // Obj(Map(a->Num(1),b->Arr(Num(12),Num(32),Obj(x->Num(123)))
        @unused def replaceFirstValue(
          k: String,
          value: Json,
        ): Json
      }
    },
    new Assignment {
      override def assigneeFullName: String = "Репінська Дарья Андріївна"
      // case class for polynomials
      // take Nat class and change it to be more memory-efficient by having
      // separate cases for 1,10, 100, 1000 etc (ie go from `Nat = Zero|Succ(nat)` to Nat=Z|Succ(nat)|One|Ten|...)
      // then write a tail-recursive method to exponentiate
      //  def pow(n: Int): Nat
      // Also, add methods to check if Nat is even, is odd
      // and then implement exponentiation via squaring (https://en.wikipedia.org/wiki/Exponentiation_by_squaring)
    },
    new Assignment {
      override def assigneeFullName: String = "Счастний Максим Валерійович"
      // //x^+5 == Poly("x", Map(2->1,0->5))
      @unused case class Poly(
        varName: String,
        coeff: Map[Int, Int],
      ) {
        @unused def add(other: Poly): Either[String, Poly] =
          ??? // if var names are not the same, return Left("Variable name mismatch")
        @unused def mul(other: Poly): Either[String, Poly] =
          ??? // if var names are not the same, return Left("Variable name mismatch")
        @unused def value(varValue: BigInt): BigInt = ???
      }

    },
    new Assignment {
      override def assigneeFullName: String = "Філін Денис Дмитрович"
      // https://en.wikipedia.org/wiki/Fixed-point_iteration
      @unused def fixedPoint(f: Double => Double): Option[Double] =
        ??? // return Left("some error message") if e.g. function doesn't converge etc
      // now implement Newton's root finding in terms of fixed point search
      // https://en.wikipedia.org/wiki/Newton%27s_method
      @unused def rootNewton(f: Double => Double): Option[Double] =
        ??? // return Left("some error message") if e.g. function doesn't converge etc
      // now,
    },
    new Assignment {
      override def assigneeFullName: String = "Фурутіна Євгенія Віталіївна"
      // = Var(name)|And(bool,bool)|Or(bool,bool)|Not(bool)|True|False
      /*sealed?*/
      @unused trait Bool {
        // https://en.wikipedia.org/wiki/Disjunctive_normal_form
        @unused def dnf(): Bool
        // https://en.wikipedia.org/wiki/Conjunctive_normal_form
        @unused def cnf(): Bool
        // example: a&(T|F)=>a etc
        @unused def eliminateConstants(): Bool
        // solve("a&(b|F)", Map(a->T,b->T)) == t
        // Left(bool) if we have more free variables, Right(true|false) if we can solve completely
        @unused def substitute(vars: Map[String, Bool]): Either[Bool, Boolean]
      }
      @unused def parse(s: String): Either[String, Bool] = ??? // use Left(...) to inform about errors
      // example: parse("(T & a)|F & !(b|c)")
    },
    BstTests,
    new Assignment {
      override def assigneeFullName: String = "Бондаренко Олександр Сергійович"
      type WageCalculator = Int => Int
      // example:
      @unused val wage: WageCalculator =
        hours => hours * 20 // total pay per day given how many hours someone worked for
      // use Left(some error message) if some employee worked for more than 10 hours
      @unused def hoursToWages(hoursPerEmployee: List[Int])(wage: WageCalculator): Either[String, List[Int]] = ???
      // now change this to accept a list of wages (different employees have different rates)
      @unused def hoursToWages(hoursPerEmployee: List[Int])(wages: List[WageCalculator]): Either[String, List[Int]] =
        ???
      // next, design a set of classes to represent hours, wages and workers so in the end we operate on something like
      // def hoursToWages(hours: List[Hour])(employees: List[Employee]): Either[String, List[EmployeePayment]]
      // next, come up with a way to avoid having separate lists? is there any data structure
      // suitable to associate employees with their working hours? refactor the code above to use this new approach
    },
    didukh.ExprTests,
    new Assignment {
      override def assigneeFullName: String = "Ісаченко Нікіта Сергійович"

      // sealed?
      @unused trait Eval[A] { // Now(a:A) | Later(a: () => A) | FlatMap(c: Compute[A], f: A => Compute[B]) | Failed(exception: Throwable)
        def valueUnsafe(): A // throws exception
        def value(): Either[Throwable, A] // throws exception

        // tailrec or even mutable as long as does not blow up the stack
        def flatMap[B](f: A => Eval[B]): Eval[B]
      }

      @unused object Eval {
        def now[A](a: A): Eval[A] = ??? // Now(a)
        def later[A](a: () => A): Eval[A] = ??? // Later(a)
      }
      // example:
      // Eval.now(1).flatMap(x => Eval.later(() => x + 1)).flatMap(x => Eval.later(() => x - 100))

      // following must compute:
      @unused object MutualRecursion {
        def even(n: Int): Eval[Boolean] =
          Eval.now(n == 0).flatMap {
            case true => Eval.now(true)
            case false => odd(n - 1)
          }

        def odd(n: Int): Eval[Boolean] =
          Eval.now(n == 0).flatMap {
            case true => Eval.now(false)
            case false => even(n - 1)
          }
      }

      // should be ok to compute
      // MutualRecursion.odd(199999).valueUnsafe()
    },
    new Assignment {
      override def assigneeFullName: String = "Леськів Василина Володимирівна"
      // https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
      // model a finite board for Game of Life (e.g. 32x32)
      // representing it as a set of "live" coordinates.
      // write a function next(board: Board): Board
      // that takes a current board and returns the next one
      // you can skip writing property-based tests but still write normal tests
      // (use Glider as a sample shape)
      // please also write a function to render a board.
      // use ascii/unicode for black/white squares, e.g.
      // "\u2B1B" (⬛)
      // and
      // "\u2B1C" (⬜) (or maybe just a space (" " or "\u0020"))
    },
    patsora.ExprTests,
  )
}
