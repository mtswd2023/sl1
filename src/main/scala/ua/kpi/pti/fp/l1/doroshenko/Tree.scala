package ua.kpi.pti.fp.l1.doroshenko

sealed trait Tree[+A] {
  override def toString: String = this match {
    case Leaf(value) => value.toString
    case Branch(left, right) => s"Branch(${left.toString}, ${right.toString})"
    case EmptyTree => "empty"
  }
}

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](
  left: Tree[A],
  right: Tree[A],
) extends Tree[A]

sealed trait Empty extends Tree[Nothing]
case object EmptyTree extends Empty

object Tree {

  def reduce[A, B](
    tree: Tree[A],
    zero: B,
  )(
    f: (A, B) => B,
  ): B = tree match {
    case Leaf(value) => f(value, zero)
    case Branch(left, right) =>
      val leftResult = reduce(left, zero)(f)
      reduce(right, leftResult)(f)
    case _ => zero
  }

  def filter[A](tree: Tree[A])(f: A => Boolean): Tree[A] = tree match {
    case Leaf(value) =>
      if (f(value)) Leaf(value)
      else EmptyTree
    case Branch(left, right) =>
      val filteredLeft = filter(left)(f)
      val filteredRight = filter(right)(f)
      if (filteredLeft == EmptyTree && filteredRight == EmptyTree) EmptyTree
      else Branch(filteredLeft, filteredRight)
    case _ => EmptyTree
  }

  def takeWhile[A](tree: Tree[A])(f: A => Boolean): Tree[A] = tree match {
    case Leaf(value) =>
      if (f(value)) Leaf(value)
      else EmptyTree
    case Branch(left, right) =>
      val leftResult = takeWhile(left)(f)
      if (leftResult == EmptyTree) EmptyTree
      else Branch(leftResult, takeWhile(right)(f))
    case _ => EmptyTree
  }

  // method is tail-recursive
  def minDepth[A](tree: Tree[A]): Int = {
    def minDepthAst(
      tree: Tree[A],
      depth: Int,
    ): Int = tree match {
      case Leaf(_) => depth + 1
      case Branch(left, right) =>
        val leftDepth = minDepthAst(left, depth + 1)
        val rightDepth = minDepthAst(right, depth + 1)
        Math.min(leftDepth, rightDepth)
      case _ => 0
    }

    minDepthAst(tree, 0)
  }

  // method is tail-recursive
  def maxDepth[A](tree: Tree[A]): Int = {
    def maxDepthAst(
      tree: Tree[A],
      depth: Int,
    ): Int = tree match {
      case Leaf(_) => depth + 1
      case Branch(left, right) =>
        val leftDepth = maxDepthAst(left, depth + 1)
        val rightDepth = maxDepthAst(right, depth + 1)
        Math.max(leftDepth, rightDepth)
      case _ => 0
    }

    maxDepthAst(tree, 0)
  }
}
