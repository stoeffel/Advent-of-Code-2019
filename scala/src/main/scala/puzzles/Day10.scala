package puzzles
import cats.effect._
import cats.syntax.all._
import scala.collection.immutable.Nil

object Day10 extends IOApp {
  final case class Astroid(x: Int, y: Int) {
    def distance(astroid: Astroid): Double =
      Math.sqrt(Math.pow(astroid.x - x, 2) + Math.pow(astroid.y - y, 2))

    def angle(astroid: Astroid): Double =
      top0(Math.toDegrees(Math.atan2(astroid.y - y, astroid.x - x)))

    private def top0(degrees: Double) =
      if (degrees < -90) {
        450 + degrees
      } else {
        degrees + 90
      }
  }

  final case class Station(astroid: Astroid, visibleAstroids: Int)

  def toAstroids(lines: List[String]): List[Astroid] =
    lines.zipWithIndex
      .collect {
        case (xs, y) =>
          xs.zipWithIndex.collect { case ('#', x) => Astroid(x, y) }
      }
      .flatten
      .toList

  def toStation(astroids: List[Astroid])(astroid: Astroid): Station =
    Station(astroid, astroids.map(astroid.angle(_)).toSet.size)

  def explode(astroids: List[List[Astroid]], count: Int): Option[Astroid] =
    (count, astroids) match {
      case (1, _)          => astroids.headOption.flatMap(_.headOption)
      case (_, head :: tl) => explode(tl :+ head.tail, count - 1)
      case (_, Nil)        => None
    }

  def orbiting(station: Station, astroids: List[Astroid]) =
    astroids
      .filter(_ != station.astroid)
      .groupBy(station.astroid.angle(_))
      .toList
      .sortBy(_._1)
      .map(_._2.sortBy(station.astroid.distance(_)))

  def run(args: List[String]): IO[ExitCode] =
    for {
      input <- Util.readFile("../input/day-10.txt")
      res <- IO {
        val astroids = toAstroids(input.getLines.toList)
        val station = astroids.map(toStation(astroids)).maxBy(_.visibleAstroids)
        val lastExplosion = explode(orbiting(station, astroids), 200)
        (station, lastExplosion)
      }
      _ <- Util.printStrLn(s"AoC 19 - Day 10:\n\n$res")
    } yield (ExitCode.Success)
}
