package ua.kpi.pti.fp.l1.Hrytsenko

sealed trait Tree[+A]
case class Leaf[+A](a: A) extends Tree[A]
case class Branch[+A](
  l: Tree[A],
  r: Tree[A],
) extends Tree[A]

class TreeOps[+A](tree: Tree[A]) {
  def map[B](f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(new TreeOps(l).map(f), new TreeOps(r).map(f))
  }

  def size: Int = {
    def helper(
      t: Tree[A],
      acc: Int,
    ): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(l, r) => helper(l, acc) + helper(r, acc)
    }

    helper(tree, 0)
  }

  def find(f: A => Boolean): Option[A] = tree match {
    case Leaf(a) => Option(a).filter(f)
    case Branch(l, r) => new TreeOps(l).find(f) orElse new TreeOps(r).find(f)
  }

  def toList: List[A] = {
    def helper(
      t: Tree[A],
      acc: List[A],
    ): List[A] = t match {
      case Leaf(a) => a :: acc
      case Branch(l, r) => helper(l, helper(r, acc))
    }

    helper(tree, List.empty[A]).reverse
  }

  def limitToDepth(n: Int): Tree[A] = {
    def helper(
      t: Tree[A],
      depth: Int,
    ): Tree[A] = {
      if (depth > n) Leaf(null.asInstanceOf[A]) // або Leaf(defaultValue), якщо є значення за замовчуванням
      else t match {
        case Leaf(a) => Leaf(a)
        case Branch(l, r) => Branch(helper(l, depth + 1), helper(r, depth + 1))
      }
    }

    helper(tree, 0)
  }

}

object Tree {
  def apply[A](tree: Tree[A]): TreeOps[A] = new TreeOps(tree)
}
