package game.logic

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/** Ці тести стосуються імен шаблонів, визначених на Wikipedia page for Conway's Game of Life.
  * See: https://en.wikipedia.org/wiki/Conway's_Game_of_Life#Examples_of_patterns *
  */
class LifeSpec extends AnyFunSpec with Matchers {

  import Life.{Cell => Cell}

  describe("для будь -якого окремо") {
    describe("коли живих клітин немає") {
      it("жоден не оживе") {
        Life.evolve(Set.empty) shouldBe empty
      }
    }

    describe("коли жива лише одна клітина") {
      it("він загине, і живих клітин не буде") {
        val cells = Set(Cell(2, 4))
        Life.evolve(cells) shouldBe empty
      }
    }

    describe("коли є дві живі клітини") {
      it("вони обидва загинуть, і живих клітин не буде") {
        val cells = Set(Cell(2, 4), Cell(2, 5))
        Life.evolve(cells) shouldBe empty
      }
    }

    describe("коли позиція кандидата має трьох живих сусідів") {
      it("нова клітина оживе на цій посаді") {
        val cells = Set(Cell(2, 1), Cell(1, 2), Cell(2, 2))
        val tick1 = Life.evolve(cells)
        tick1 should contain(Cell(1, 1))
      }
    }

    describe("натюрмові структури не повинні змінюватися") {
      describe("отже, коли є клітини, які утворюють малюнок \"човен\"") {
        it("жодна з клітин не гине, і жодна нова клітини не народжується") {
          val boat =
            Set(Cell(2, 2), Cell(3, 2), Cell(2, 3), Cell(4, 3), Cell(3, 4))

          val result = Life.evolve(boat)
          boat shouldEqual result
        }
      }

      describe(
        "так само, коли є клітини, які утворюють малюнок \"вулика\""
      ) {
        it("жодна з клітин не гине, і жодна нова клітини не народжується") {
          val beehive = Set(
            Cell(3, 2),
            Cell(4, 2),
            Cell(2, 3),
            Cell(5, 3),
            Cell(3, 4),
            Cell(4, 4)
          )

          val result = Life.evolve(beehive)
          beehive shouldEqual result
        }
      }
    }
  }

  describe("коли відбувається кілька кліщів") {
    describe("деякі закономірності будуть коливатися між кількома") {
      describe("отже, коли з’являється «блискучий» візерунок") {
        val start = Set(Cell(2, 3), Cell(3, 3), Cell(4, 3))
        val expected = Set(Cell(3, 2), Cell(3, 3), Cell(3, 4))
        val tick1 = Life.evolve(start)
        val tick2 = Life.evolve(tick1)

        it("перший галочок створить три живі клітини") {
          tick1 should have size 3
        }

        it("де дві клітини гинуть і народжуються дві нові клітини") {
          (start diff tick1) should have size 2
        }

        it("і шаблон відповідає очікуваним коливанням") {
          tick1 shouldEqual expected
        }

        it("потім другий повертає клітини у своїх початкових положеннях") {
          tick2 shouldEqual start
        }
      }

      describe("або якщо з’являється схема \"жаби\"") {
        val start = Set(
          Cell(3, 3),
          Cell(4, 3),
          Cell(5, 3),
          Cell(2, 4),
          Cell(3, 4),
          Cell(4, 4)
        )

        val expected = Set(
          Cell(4, 2),
          Cell(2, 3),
          Cell(5, 3),
          Cell(2, 4),
          Cell(5, 4),
          Cell(3, 5)
        )

        val tick1 = Life.evolve(start)
        val tick2 = Life.evolve(tick1)

        it("перший створить шість живих клітин") {
          tick1 should have size 6
        }

        it("і шаблон відповідає очікуваним коливанням") {
          tick1 shouldEqual expected
        }

        it(
          "потім другий галочок повертає клітини у своїх початкових положеннях"
        ) {
          tick2 shouldEqual start
        }
      }
    }
  }
}
