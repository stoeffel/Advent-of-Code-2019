package puzzles
import cats.effect.IO
import cats.implicits._
import cats.implicits._
import scala.collection.immutable.Nil
import cats.effect.ContextShift
import cats.data.NonEmptyList
import cats.syntax.all._

object Amp {
  def possiblePhaseSettings(from: Int, to: Int): NonEmptyList[List[Int]] = {
    val range = Range(from, to + 1)
    NonEmptyList
      .fromList(range.permutations.toList.map(_.toList))
      .getOrElse(NonEmptyList.of(range.toList))
  }

  def maxThrusterSignal(from: Int, to: Int, code: Array[Int]): IO[Int] =
    possiblePhaseSettings(from, to)
      .traverse(testSetting(code, None, _))
      .map(_.toList.flatten.max)

  def testSetting(
      code: Array[Int],
      prevOut: Option[Int],
      settings: List[Int]
  ): IO[Option[Int]] = {
    var calls = 0
    settings match {
      case Nil => IO(prevOut)
      case head :: tl =>
        TESTIntCode
          .diagnostic(
            IO {
              if (calls == 0) {
                calls = 1
                head
              } else {
                prevOut.getOrElse(0)
              }
            },
            code
          )
          .map(_.headOption)
          .flatMap(testSetting(code, _, tl))
    }
  }
}
