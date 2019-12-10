package puzzles
import cats.effect.IO
import puzzles.Op._
import puzzles.Op.Terminate._

object IntCode {
  def parse(input: String): IO[Memory] = Memory.parse(input)

  def run(
      input: IO[Code],
      xs: Memory,
      pos: Position = Position(0)
  ): IO[List[Code]] =
    for {
      res <- diagnostic(input, xs, pos)
      out <- res match {
        case UnknownOp(e) => throw (new Error(s"Unknown op $e"))
        case End()        => IO(List())
        case Output(newOut, newCode, newPos) =>
          run(input, newCode, newPos)
            .map(xs => newOut +: xs)
      }
    } yield out

  /**
    * >>> import cats.effect.IO
    * >>> import puzzles.Op.Memory
    * >>> IntCode.diagnostic(IO(0), Memory.fromList(List(11002,5,3,0,99,33))).unsafeRunSync
    * End()
   **/
  def diagnostic(
      input: IO[Code],
      xs: Memory,
      pos: Position = Position(0)
  ): IO[Terminate] =
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
