package ua.kpi.pti.fp.l1.assignment.Hrytsenko
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import org.scalacheck.Gen
import ua.kpi.pti.fp.l1.Hrytsenko.{Tree, Leaf, Branch}
import ua.kpi.pti.fp.l1.assignment.Assignment
case object TreeHTest extends Assignment {

  override def assigneeFullName: String = "Гриценко Марія Дмитрівна"

  object TreeHTest extends Properties("Tree") {

    val genLeaf: Gen[Leaf[Int]] = Gen.chooseNum(Int.MinValue, Int.MaxValue).map(Leaf(_))

    def genBranch(depth: Int): Gen[Branch[Int]] = {
      if (depth <= 0) genLeaf.map(l => Branch(l, l))
      else for {
        left <- genTree(depth - 1)
        right <- genTree(depth - 1)
      } yield Branch(left, right)
    }

    def genTree(depth: Int): Gen[Tree[Int]] = Gen.oneOf(genLeaf, genBranch(depth))

    // Тест для методу size()
    property("size") = forAll(genTree(5)) { tree =>
      val treeSize = Tree(tree).size
      val calculatedSize = calculateSize(tree)
      treeSize == calculatedSize
    }

    // Перевіряємо, чи повертає метод `size` правильний розмір для порожнього дерева
    property("sizeEmptyTree") = {
      val emptyTree = Leaf(0)
      val ops = Tree(emptyTree)
      val treeSize = ops.size
      treeSize == 1
    }

    // Тест для методу find()
    property("find") = forAll(genTree(5), Gen.function1[Int, Boolean](Gen.oneOf(true, false))) {
      (
        tree,
        predicate,
      ) =>
        val ops = Tree(tree)
        val foundValue = ops.find(predicate)
        val expectedValue = findInTree(tree, predicate)
        foundValue == expectedValue
    }

    // Тест для методу toList()
    property("toList") = forAll(genTree(5)) { tree =>
      val treeList = Tree(tree).toList
      val expectedList = flattenTreeToList(tree)
      treeList == expectedList
    }

    // Перевіряємо, чи повертає метод `toList` порожній список для порожнього дерева
    property("toListEmptyTree") = {
      val emptyTree = Leaf(0)
      val ops = Tree(emptyTree)
      val treeList = ops.toList
      treeList.isEmpty
    }

    // Тест для методу limitToDepth()
    property("limitToDepth") = forAll(genTree(5), Gen.chooseNum(0, 5)) {
      (
        tree,
        depth,
      ) =>
        val limitedTree = Tree(tree).limitToDepth(depth)
        val expectedTree = limitTreeToDepth(tree, depth)
        limitedTree == expectedTree
    }

    // Тест для методу map()
    property("map") = forAll(genTree(5), Gen.function1[Int, Int](Gen.chooseNum(0, 10))) {
      (
        tree,
        f,
      ) =>
        val ops = Tree(tree)
        val mappedTree = ops.map(f)
        val expectedTree = mapTree(tree, f)
        mappedTree == expectedTree
    }

    // Перевіряємо, чи правильно працює метод `map` при застосуванні функції, що повертає різні типи
    property("mapWithDifferentTypes") = forAll(genTree(5), Gen.function1[Int, String](Gen.alphaNumStr)) {
      (
        tree,
        f,
      ) =>
        val ops = Tree(tree)
        val mappedTree = ops.map(f)
        val expectedTree = mapTree(tree, f)
        mappedTree == expectedTree
    }

    // Внутрішні допоміжні функції для обчислення очікуваних результатів

    def calculateSize[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => calculateSize(l) + calculateSize(r)
    }

    def findInTree[A](
                       tree: Tree[A],
                       predicate: A => Boolean,
                     ): Option[A] = tree match {
      case Leaf(a) => Some(a).filter(predicate)
      case Branch(l, r) => findInTree(l, predicate) orElse findInTree(r, predicate)
    }

    def flattenTreeToList[A](tree: Tree[A]): List[A] = tree match {
      case Leaf(a) => List(a)
      case Branch(l, r) => flattenTreeToList(l) ++ flattenTreeToList(r)
    }

    def limitTreeToDepth[A](
                             tree: Tree[A],
                             depth: Int,
                           ): Tree[A] = {
      if (depth <= 0) Leaf(null.asInstanceOf[A])
      else tree match {
        case Leaf(a) => Leaf(a)
        case Branch(l, r) => Branch(limitTreeToDepth(l, depth - 1), limitTreeToDepth(r, depth - 1))
      }
    }

    def mapTree[A, B](
                       tree: Tree[A],
                       f: A => B,
                     ): Tree[B] = tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(mapTree(l, f), mapTree(r, f))
    }

  }
}
