package puzzles
import scala.io.{Codec, BufferedSource, Source}
import java.io.File
import scala.util.Try
import cats._
import cats.implicits._
import puzzles.implicits._
import cats.effect.IO
import puzzles.Op2.Error.{UnknownOp, EndOfInput}

object TESTIntCode extends IntCode {
  final case class Config(input: Int, pos: Int, xs: Array[Int], out: List[Int])
  def run(config: Config): List[Int] =
    Op2
      .fromIntCode(config.input, config.pos, config.xs)
      .map(_.exec(config.pos, config.xs)) match {
      case Right(Some((newPos, ys, newOut))) =>
        run(
          config.copy(
            pos = newPos,
            xs = ys,
            out = newOut match {
              case Some(o) => o :: config.out
              case None    => config.out
            }
          )
        )
      case Left(UnknownOp(op)) => throw (new Error(s"Unknown op: $op"))
      case _                   => config.out
    }

  /**
    * {{{
    * >>> TESTIntCode.diagnostic(0, Array(11002,5,3,0,99,33))
    * List()
    *
    * >>> TESTIntCode.diagnostic(0, Array(11002,7,3,0,4,0,99,33))
    * List(99)
    * }}}
   **/
  def diagnostic(input: Int, xs: Array[Int]) = {
    run(Config(input, 0, xs, List())).reverse
  }
}
