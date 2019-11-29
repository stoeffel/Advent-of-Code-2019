package puzzles
import scala.io.Source
import scala.util.Try

object Part1 extends App {
  val result = Source
    .fromFile("../../input/day-1.txt")
    .getLines
    .foldLeft(0) { (acc, massStr) =>
      Try(massStr.toInt).toOption match {
        case None       => acc
        case Some(mass) => acc + (mass / 3 - 2)
      }
    }
  println(s"AoC 19 - Day 1: $result")
}

object Part2 extends App {
  def calc(mass: Int): Int =
    mass / 3 - 2 match {
      case fuel if fuel <= 0 => 0
      case fuel              => fuel + calc(fuel)
    }
  val result = Source
    .fromFile("../../input/day-1.txt")
    .getLines
    .foldLeft(0) { (acc, massStr) =>
      Try(massStr.toInt).toOption match {
        case None       => acc
        case Some(mass) => acc + calc(mass)
      }
    }
  println(s"AoC 19 - Day 1: $result")
}
