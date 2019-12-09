package puzzles
import cats.effect._
import cats.syntax.all._

object Day8 extends IOApp {
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
      }.mkString)
      .mkString("\n")

  def run(args: List[String]): IO[ExitCode] =
    for {
      input <- Util.readFile("../input/day-8.txt")
      layers_ <- IO { layers(input.getLines.toList, 25, 6) }
      checksum_ <- IO { checksum(layers_) }
      _ <- Util.printStrLn(s"AoC 19 - Day 8: $checksum_")
      image <- IO { render(layers_, 25) }
      _ <- Util.printStrLn(s"AoC 19 - Day 8:\n\n$image")
    } yield (ExitCode.Success)
}
