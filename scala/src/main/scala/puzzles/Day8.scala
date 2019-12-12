package puzzles
import cats.effect._
import cats.syntax.all._
import puzzles.implicits._

object Day8 extends AoCApp {
  type Layer = List[Int]

  def layers(lines: List[String], width: Int, height: Int) =
    lines
      .flatMap(_.toCharArray)
      .map(_.toString.toInt)
      .grouped(width * height)
      .toList

  def checksum(layers: List[Layer]): Int = {
    val fewestZeros = layers.minBy(_.count(_ == 0))
    fewestZeros.count(_ == 1) * fewestZeros.count(_ == 2)
  }

  def render(layers: List[Layer], width: Int) =
    layers.transpose
      .map(_.filter(_ != 2).head)
      .grouped(width)
      .map(_.map {
        case 0 => " "
        case x => x.toString
      })
      .toList
      .renderGrid

  def run()(implicit cs: ContextShift[IO]) =
    for {
      input <- Util.readFile("../input/day-8.txt")
      layers_ <- IO { layers(input.getLines.toList, 25, 6) }
      checksum_ <- IO { checksum(layers_) }
      _ <- Util.printStrLn(s"Part 1: $checksum_")
      image <- IO { render(layers_, 25) }
      _ <- Util.printStrLn(s"Part 2:\n\n$image")
    } yield ()
}
