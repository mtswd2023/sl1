package ua.kpi.pti.fp.l1.balatska
import ua.kpi.pti.fp.l1.balatska.Bst.Empty
import ua.kpi.pti.fp.l1.balatska.Bst.Node

sealed trait Bst {
  private def findNode(key: Double): Bst = {
    this match {
      case Node(value, _, left, right) => if (value < key) {
        right.findNode(key)
      } else if (value > key) {
        left.findNode(key)
      } else {
        this
      }
      case Empty => Bst.Empty
    }
  }

  /*override def toString(): String = {
    ""
  }*/


  //The left child is indented with "┌──────" and the right child with "└──────". 
override def toString: String = {
  def toStringHelper(node: Bst, indent: String, isLast: Boolean, isRoot: Boolean): String = {
    node match {
      case Empty => ""
      case Node(value, label, left, right) =>
        val nodeStr = {
          if (isRoot)
            s"$indent$value($label)\n"
          else
            s"$indent${if (isLast) "└────── " else "┌────── "}$value($label)\n"
        }
        val childIndent = indent + (if (isLast) "        " else "|       ")
        val leftStr = toStringHelper(left, childIndent, isLast = false, isRoot = false)
        val rightStr = toStringHelper(right, childIndent, isLast = true, isRoot = false)
        leftStr + nodeStr + rightStr
    }
  }

  toStringHelper(this, "", isLast = true, isRoot = true)
}



  def contains(a: Double): Boolean = {
    findNode(a) match {
      case Empty => false
      case Node(_, _, _, _) => true
    }
  }
  def labelOf(a: Double): Option[String] = {
    findNode(a) match {
      case Empty => None
      case Node(_, label, _, _) => Some(label)
    }
  }
  def valueOf(label: String): Option[Double] = {
    this match {
      case Empty => Option.empty
      case Node(value, label1, left, right) => if (label != label1) {
        val findLeft = left.valueOf(label)
        findLeft match {
          case None => right.valueOf(label)
          case Some(_) => findLeft
        }
      } else {
        Some(value)
      }
    }
  }

def insert(a: Double, label: String): Bst = this match {
  case Empty => Bst.Node(a, label, Bst.Empty, Bst.Empty)
  case Node(value, label1, left, right) =>
    if (a < value)
      Bst.Node(value, label1, left.insert(a, label), right)
    else if (a > value)
      Bst.Node(value, label1, left, right.insert(a, label))
    else
      this
}


  def toList(): List[Double] = {
    def inorderTraversal(node: Bst): List[Double] = {
      node match {
        case Empty => List()
        case Node(value, _, left, right) => inorderTraversal(left) ::: List(value) ::: inorderTraversal(right)
      }
    }

    inorderTraversal(this)
  }
}

object Bst {
  case object Empty extends Bst
  case class Node(value: Double, label: String, left: Bst, right: Bst) extends Bst

  //def create(xs: List[(Double, String)]): Bst = ???

 def create(xs: List[(Double, String)]): Bst = {
    def insertAll(tree: Bst, elements: List[(Double, String)]): Bst = elements match {
      case Nil => tree
      case (value, label) :: tail => insertAll(tree.insert(value, label), tail)
    }

    insertAll(Empty, xs)
  }

}
