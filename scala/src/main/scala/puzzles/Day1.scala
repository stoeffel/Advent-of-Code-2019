package puzzles
import scala.io.{Codec, BufferedSource, Source}
import java.io.File
import scala.util.Try
import cats._
import cats.implicits._
import cats.effect.IO

object Part1 extends App {
  def printStrLn(msg: String): IO[Unit] = IO(println(msg))
  def readFile(path: String)(implicit codec: Codec): IO[BufferedSource] =
    IO(Source.fromFile(path))

  val result = for {
    file <- readFile("../input/day-1.txt")
    lines = file.getLines
    fuel = lines.foldLeft(0) { (acc, massStr) =>
      Try(massStr.toInt).toOption match {
        case None       => acc
        case Some(mass) => acc + (mass / 3 - 2)
      }
    }
  } yield fuel
  result.flatMap(r => printStrLn(s"AoC 19 - Day 1: $r")).unsafeRunSync
}

object Part2 extends App {
  def calc(mass: Int): Int =
    mass / 3 - 2 match {
      case fuel if fuel <= 0 => 0
      case fuel              => fuel + calc(fuel)
    }
  val result = Source
    .fromFile("../input/day-1.txt")
    .getLines
    .foldLeft(0) { (acc, massStr) =>
      Try(massStr.toInt).toOption match {
        case None       => acc
        case Some(mass) => acc + calc(mass)
      }
    }
  println(s"AoC 19 - Day 1: $result")
}
