package puzzles
import puzzles.Op2.Terminate.{UnknownOp, End}
import cats.effect.IO

object TESTIntCode extends IntCode {
  final case class Config(
      input: IO[Int],
      output: Int => IO[Unit],
      pos: Int,
      xs: Array[Int]
  )
  def run(config: Config): IO[Unit] =
    Op2
      .fromIntCode(config.input, config.output, config.pos, config.xs)
      .exec(config.pos, config.xs)
      .flatMap(
        result =>
          result match {
            case Left(UnknownOp(op)) => throw (new Error(s"Unknown op: $op"))
            case Left(End())         => IO.unit
            case Right(next) =>
              run(
                config.copy(
                  pos = next.pos,
                  xs = next.xs
                )
              )
          }
      )

  /**
    * >>> import cats.effect.IO
    * >>> var result1 : Option[Int] = None
    * ... def setResult1(x: Int) = {
    * ...   result1 = Some(x)
    * ...   IO.unit
    * ... }
    * ... TESTIntCode.diagnostic(IO(0), setResult1, Array(11002,5,3,0,99,33)).unsafeRunSync
    * >>> result1
    * None
    *
    * >>> var result2 : Option[Int] = None
    * ... def setResult2(x: Int) = {
    * ...   result2 = Some(x)
    * ...   IO.unit
    * ... }
    * ... TESTIntCode.diagnostic(IO(0), setResult2, Array(11002,7,3,0,4,0,99,33)).unsafeRunSync
    * >>> result2
    * Some(99)
   **/
  def diagnostic(
      input: IO[Int],
      output: Int => IO[Unit],
      xs: Array[Int]
  ): IO[Unit] = {
    run(Config(input, output, 0, xs))
  }
}
