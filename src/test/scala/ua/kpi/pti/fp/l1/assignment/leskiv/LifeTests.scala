package ua.kpi.pti.fp.l1.assignment.leskiv

import ua.kpi.pti.fp.l1.assignment.Assignment
import ua.kpi.pti.fp.l1.assignment.L1PropOrTest
import ua.kpi.pti.fp.l1.leskiv.Life

import org.scalacheck.Prop.{forAll}
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

case object LifeTests extends Assignment {
  override def assigneeFullName: String = "Леськів Василина Володимирівна"

  // Define a generator for Cell objects
  implicit val cellArbitrary: Arbitrary[Life.Cell] = Arbitrary {
    for {
      x <- Gen.choose(Long.MinValue, Long.MaxValue)
      y <- Gen.choose(Long.MinValue, Long.MaxValue)
    } yield Life.Cell(x, y)
  }

  // Property-based test to check if evolve() returns a valid set of cells
  def evolveReturnsValidCells: L1PropOrTest = {
    val prop = forAll { (cells: Set[Life.Cell]) =>
      val evolvedCells = Life.evolve(cells)
      val allCellsAreValid = evolvedCells.forall(cell => cell.x >= Long.MinValue && cell.x <= Long.MaxValue && cell.y >= Long.MinValue && cell.y <= Long.MaxValue)
      allCellsAreValid
    }
    L1PropOrTest.L1Prop(prop)
  }

  // Property-based test to check if evolve() maintains live cells with 2 or 3 live neighbors
  def evolveMaintainsLiveCells: L1PropOrTest = {
    val prop = forAll { (cells: Set[Life.Cell]) =>
      val evolvedCells = Life.evolve(cells)
      val liveCells = evolvedCells.filter(cells.contains)
      val liveCellsHaveValidNeighbors = liveCells.forall { liveCell =>
        val liveNeighbors = Life.liveNeighbours(liveCell, cells)
        val liveNeighborsCount = liveNeighbors.size
        (liveNeighborsCount == 2 || liveNeighborsCount == 3)
      }
      liveCellsHaveValidNeighbors
    }
    L1PropOrTest.L1Prop(prop)
  }

  override def props: List[(String, L1PropOrTest)] = List(
    "evolveReturnsValidCells" -> evolveReturnsValidCells,
    "evolveMaintainsLiveCells" -> evolveMaintainsLiveCells
  )
}
