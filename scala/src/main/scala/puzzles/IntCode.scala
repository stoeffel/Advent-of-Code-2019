package puzzles
import cats.effect.IO
import puzzles.Op.Terminate
import puzzles.Op.Terminate._

object IntCode extends BasicIntCode {
  /**
    * >>> import cats.effect.IO
    * >>> IntCode.diagnostic(IO(0), Array(11002,5,3,0,99,33)).unsafeRunSync
    * End()
    *
   **/
  def diagnostic(input: IO[Int], xs: Array[Int], pos: Int = 0): IO[Terminate] =
    Op.fromIntCode(input, pos, xs)
      .exec(pos, xs)
      .flatMap(
        result =>
          result match {
            case Left(halt)  => IO(halt)
            case Right(next) => diagnostic(input, next.xs, next.pos)
          }
      )
}
