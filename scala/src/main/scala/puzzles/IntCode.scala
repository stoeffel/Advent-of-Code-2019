package puzzles
import scala.io.{Codec, BufferedSource, Source}
import java.io.File
import scala.util.Try
import cats._
import cats.implicits._
import puzzles.implicits._
import puzzles.IntCode.NounVerb._
import cats.effect.IO
import puzzles.Op.Error.{UnknownOp, EndOfInput}

object IntCode {
  def restoreGravityAssist(xs: Array[Int], nounVerb: NounVerb): Array[Int] =
    xs.updated(1, nounVerb.noun).updated(2, nounVerb.verb)

  def run(pos: Int, xs: Array[Int]): Int =
    Op.fromIntCode(pos, xs)
      .map(_.exec(xs)) match {
      case Right(Some((arity, ys))) => run(pos + arity + 1, ys)
      case Left(UnknownOp(op))      => throw (new Error(s"Unknown op: $op"))
      case _                        => xs(0)
    }

  object NounVerb {
    final case class NounVerb(noun: Int, verb: Int)

    def default(): NounVerb = NounVerb(noun = 12, verb = 2)

    def build(): Traversable[NounVerb] =
      Range(0, 99)
        .cross(Range(0, 99))
        .map { case (noun, verb) => NounVerb(noun, verb) }

    def toInt(nv: NounVerb): Int = 100 * nv.noun + nv.verb
  }

  def determinePair(goal: Int, xs: Array[Int]): Option[Int] = {
    NounVerb.build
      .find(
        (restoreGravityAssist(xs, _))
          andThen (run(0, _))
          andThen (_ == goal)
      )
      .map(NounVerb.toInt(_))
  }

  def parse(input: String): IO[Array[Int]] =
    Util
      .readFile(input)
      .map(
        _.getLines
          .flatMap(_.split(","))
          .map { code =>
            Try(code.toInt).toOption
          }
          .collect { case Some(code) => code }
          .toArray
      )
}
