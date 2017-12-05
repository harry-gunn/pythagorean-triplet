package pythagorean.triplet

import scala.collection.immutable.IndexedSeq
import scala.math.sqrt

object PythagoreanTriplet extends App {

  println(findSolution(1000).toString())

  def findSolution(total: Int) = {
    pythagoreanTriplets(1, total)
      .filter(sumOfTriplets(_) == total)
      .map(numbers => Solution(numbers, productOfTriplets(numbers)))
      .toList
  }

  def sumOfTriplets(numbers: (Int, Int, Int)) = numbers.productIterator.toList.asInstanceOf[List[Int]].sum

  def productOfTriplets(numbers: (Int, Int, Int)) = numbers.productIterator.toList.asInstanceOf[List[Int]].product

  def isPythagorean(numbers: (Int, Int, Int)) = {
    val numberList: List[Int] = List(numbers._1, numbers._2, numbers._3)
    val hypotenuse = numberList.max
    val legs: List[Int] = numberList.filterNot(x => x == hypotenuse)

    legs.length match {
      case 2 => sqr(hypotenuse) - (sqr(legs(0))+ sqr(legs(1))) == 0
      case _ => false
    }
  }

  def pythagoreanTriplets(a: Int, b: Int): IndexedSeq[(Int, Int, Int)] = {
    for {
      x <- a to b - 2
      y <- x until b
      target = sqr(x) + sqr(y)
      z = sqrt(target).toInt
      if z <= b && sqr(z) == target
    } yield (x, y, z)
  }

  def sqr(x: Int) = x * x
}
