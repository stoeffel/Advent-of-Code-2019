package puzzles
import scala.io.{Codec, BufferedSource, Source}
import java.io.File
import scala.util.Try
import cats._
import cats.implicits._
import cats.effect._

object Day1 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      _ <- part1
      _ <- part2
    } yield ()

  def part1() =
    for {
      file <- Util.readFile("../input/day-1.txt")
      lines = file.getLines
      fuel = lines.foldLeft(0) { (acc, massStr) =>
        Try(massStr.toInt).toOption match {
          case None       => acc
          case Some(mass) => acc + (mass / 3 - 2)
        }
      }
      _ <- Util.printStrLn(s"Part 1: $fuel")
    } yield ()

  def calc(mass: Int): Int =
    mass / 3 - 2 match {
      case fuel if fuel <= 0 => 0
      case fuel              => fuel + calc(fuel)
    }
  def part2() =
    for {
      file <- Util.readFile("../input/day-1.txt")
      lines = file.getLines
      result = lines.foldLeft(0) { (acc, massStr) =>
        Try(massStr.toInt).toOption match {
          case None       => acc
          case Some(mass) => acc + calc(mass)
        }
      }
      _ <- Util.printStrLn(s"Part 2: $result")
    } yield ()
}
