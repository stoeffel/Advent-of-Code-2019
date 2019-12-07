package puzzles
import puzzles.implicits._
import scala.collection.immutable.HashMap

object Orbit {
  def fromList(input: List[String]) =
    Orbits(HashMap(input.map(parse _): _*))

  def parse(input: String) =
    input
      .split("\\)")
      .toIterator
      .exactlyTwo match { case (a, b) => b -> a }

  case class Orbits(orbits: HashMap[String, String]) {
    def count(): Int = orbits.foldLeft(0)(countHelp)

    private def countHelp(acc: Int, ab: (String, String)): Int =
      orbits.get(ab._2) match {
        case None    => acc + 1
        case Some(x) => countHelp(acc + 1, (ab._2, x))
      }

    def countFromTo(from: String, to: String): Int =
      countFromToHelp(path(from).tail, path(to))

    def path(to: String): List[String] =
      orbits.get(to) match {
        case None    => List(to)
        case Some(x) => to :: path(x)
      }

    def countFromToHelp(from: List[String], to: List[String]): Int =
      from.headOption match {
        case None => 0
        case Some(value) if (to.contains(value)) =>
          to.indexOf(value) - 1
        case Some(value) =>
          1 + countFromToHelp(from.tail, to)
      }
  }
}
