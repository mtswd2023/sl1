package ua.kpi.pti.fp.l1.assignment.Doroshenko

import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}
//import ua.kpi.pti.fp.l1.doroshenko.{EmptyTree, Tree}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.doroshenko.{Branch, Leaf, Tree}

case object TreeTests extends Assignment {

  override def assigneeFullName: String = "Дорошенко Юрій Олександрович"

  val leaf = Leaf(10)
  val branch = Branch(Leaf(20), Leaf(30))
  val nestedBranch = Branch(Leaf(40), Branch(Leaf(50), Leaf(60)))

  override val props: List[(String, L1PropOrTest)] = {

    List(
      "Filter test on leaf node - positive condition failed" -> L1SimpleTest.of {

        assert(Tree.filter(leaf)(_ > 5) == leaf)

      },
      "Reduce test on leaf node failed" -> L1SimpleTest.of {

        assert(Tree.reduce(leaf, 0)(_ + _) == 10)

      },
      "TakeWhile test on leaf node - positive condition failed" -> L1SimpleTest.of {

        assert(Tree.takeWhile(leaf)(_ < 15) == leaf)

      },
      "minDepth test on leaf node failed" -> L1SimpleTest.of {

        assert(Tree.minDepth(leaf) == 1)

      },
      "maxDepth test on leaf node failed" -> L1SimpleTest.of {

        assert(Tree.maxDepth(leaf) == 1)

      },
    )
  }

}
