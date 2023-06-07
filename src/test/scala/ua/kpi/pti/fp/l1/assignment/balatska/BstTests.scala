package ua.kpi.pti.fp.l1.assignment.balatska

import org.scalacheck.{Gen, Prop}
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest._
import ua.kpi.pti.fp.l1.assignment.{Assignment, L1PropOrTest}

import ua.kpi.pti.fp.l1.balatska.Bst

case object BstTests extends Assignment {
  override def assigneeFullName: String = "Балацька Вікторія Віталіївна"

  override def props: List[(String, L1PropOrTest)] = {
    val arbitraryBst = Gen.listOf(Gen.choose(0.0, 100.0).flatMap(value => Gen.alphaStr.map(label => (value, label)))).map(Bst.create)

    List(
      "contains returns true for inserted elements" -> L1Prop(
        Prop.forAll(arbitraryBst, Gen.choose(0.0, 100.0)) { (bst: Bst, value: Double) =>
          val updatedBst = bst.insert(value, value.toString)
          updatedBst.contains(value)
        }
      ),
      "contains returns false for non-inserted elements" -> L1Prop(
        Prop.forAll(arbitraryBst, Gen.choose(0.0, 100.0)) { (bst: Bst, value: Double) =>
          !bst.contains(value)
        }
      ),
      "labelOf returns Some(label) for inserted elements" -> L1Prop(
        Prop.forAll(arbitraryBst, Gen.choose(0.0, 100.0)) { (bst: Bst, value: Double) =>
          val updatedBst = bst.insert(value, value.toString)
          updatedBst.labelOf(value) == Some(value.toString)
        }
      ),
      "labelOf returns None for non-inserted elements" -> L1Prop(
        Prop.forAll(arbitraryBst, Gen.choose(0.0, 100.0)) { (bst: Bst, value: Double) =>
          bst.labelOf(value) == None
        }
      ),
      "valueOf returns None for non-inserted labels" -> L1Prop(
        Prop.forAll(arbitraryBst, Gen.alphaStr) { (bst: Bst, label: String) =>
          bst.valueOf(label) == None
        }
      ),
      "toList returns a sorted list of all elements" -> L1Prop(
        Prop.forAll(arbitraryBst) { bst: Bst =>
          val sortedList = bst.toList().sorted
          sortedList == sortedList.distinct && sortedList == bst.toList()
        }
      ),
      "toString creates a tree-like string representation" -> L1Prop(
        Prop.forAll(arbitraryBst) { bst: Bst =>
          val bstString = bst.toString
          // Perform some basic checks on the string format
          bstString.contains("┌──────") &&
          bstString.contains("└──────") &&
          bstString.contains("└──────") &&
          bstString.contains("│       ") &&
          bstString.contains("        ") &&
          bstString.contains("│       ") &&
          bstString.contains("        ")
        }
      )
    )
  }
}
