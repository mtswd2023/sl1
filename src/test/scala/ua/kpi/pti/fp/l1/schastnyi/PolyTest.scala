package ua.kpi.pti.fp.l1.assigment.schastnyi

import org.junit.Assert._
object PolyTest {
  def main(args: Array[String]): Unit = {
    // Тести для методу add()
    val poly1 = Poly("x", Map(2 -> 1, 0 -> 5))
    val poly2 = Poly("x", Map(1 -> 2, 0 -> 3))
    val sumResult = poly1.add(poly2)
    assertEquals(Right(Poly("x", Map(0 -> 8, 1 -> 2, 8 -> 1))), sumResult)
    println(sumResult)  // Очікуваний результат: Right(Poly(x,Map(1 -> 2, 0 -> 8, 2 -> 1)))

    val poly3 = Poly("x", Map(2 -> 1, 0 -> 5))
    val poly4 = Poly("y", Map(2 -> 3, 1 -> 2))
    val sumError = poly3.add(poly4)
    assertEquals(Left("Variable name mismatch"), sumError)
    println(sumError)  // Очікуваний результат: Left(Variable name mismatch)

    // Тести для методу mul()
    val poly5 = Poly("x", Map(1 -> 2, 0 -> 3))
    val poly6 = Poly("x", Map(2 -> 1, 0 -> 5))
    val mulResult = poly5.mul(poly6)
    assertEquals(Right(Poly("x",Map(3 -> 2, 1 -> 10, 2 -> 3, 0 -> 15))), mulResult)

    println(mulResult)  // Очікуваний результат: Right(Poly(x,Map(3 -> 2, 1 -> 10, 2 -> 3, 0 -> 15)))

    val poly7 = Poly("x", Map(1 -> 2, 0 -> 3))
    val poly8 = Poly("y", Map(2 -> 1, 1 -> 2))
    val mulError = poly7.mul(poly8)
    assertEquals(Left("Variable name mismatch"), mulError)

    println(mulError)  // Очікуваний результат: Left(Variable name mismatch)

    // Тести для методу value()
    val poly9 = Poly("x", Map(2 -> 1, 0 -> 5))
    val valueResult1 = poly9.value(2)
    assertEquals(9, valueResult1)

    println(valueResult1)  // Очікуваний результат: 9

    
  }
}
