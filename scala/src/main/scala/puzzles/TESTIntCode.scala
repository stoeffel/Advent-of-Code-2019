package puzzles
import puzzles.Op2.Terminate.{UnknownOp, End}
import cats.effect.IO

object TESTIntCode extends IntCode {
  final case class Config(
      input: IO[Int],
      pos: Int,
      xs: Array[Int],
      out: List[Int]
  )
  def run(config: Config): IO[List[Int]] =
    Op2
      .fromIntCode(config.input, config.pos, config.xs)
      .exec(config.pos, config.xs)
      .flatMap(
        result =>
          result match {
            case Left(UnknownOp(op)) => throw (new Error(s"Unknown op: $op"))
            case Left(End())         => IO { config.out }
            case Right((newPos, ys, newOut)) =>
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
          }
      )

  /**
    * >>> import cats.effect.IO
    * >>> TESTIntCode.diagnostic(IO(0), Array(11002,5,3,0,99,33)).unsafeRunSync
    * List()
    *
    * >>> TESTIntCode.diagnostic(IO(0), Array(11002,7,3,0,4,0,99,33)).unsafeRunSync
    * List(99)
   **/
  def diagnostic(input: IO[Int], xs: Array[Int]): IO[List[Int]] = {
    run(Config(input, 0, xs, List())).map(_.reverse)
  }
}
