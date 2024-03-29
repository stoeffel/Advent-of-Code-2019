package puzzles
import scala.util.Try
import puzzles.implicits._
import puzzles.BasicIntCode.NounVerb._
import cats.effect.IO
import puzzles.BasicOp.Error.{UnknownOp, EndOfInput}

abstract class BasicIntCode {
  def parse(input: String): IO[Array[Int]] =
    Util
      .readFile(input)
      .map(
        _.getLines
          .flatMap(_.split(","))
          .flatMap { code =>
            Try(code.toInt).toOption
          }
          .toArray
      )
}

object BasicIntCode extends BasicIntCode {
  def restoreGravityAssist(xs: Array[Int], nounVerb: NounVerb): Array[Int] =
    xs.updated(1, nounVerb.noun).updated(2, nounVerb.verb)

  def run(pos: Int, xs: Array[Int]): Int =
    BasicOp
      .fromIntCode(pos, xs)
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
}
