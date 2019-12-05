package puzzles
import puzzles.Op2.Terminate.{UnknownOp, End}

object TESTIntCode extends IntCode {
  final case class Config(input: Int, pos: Int, xs: Array[Int], out: List[Int])
  def run(config: Config): List[Int] =
    Op2
      .fromIntCode(config.input, config.pos, config.xs)
      .exec(config.pos, config.xs) match {
      case Left(UnknownOp(op)) => throw (new Error(s"Unknown op: $op"))
      case Left(End())         => config.out
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

  /**
    * >>> TESTIntCode.diagnostic(0, Array(11002,5,3,0,99,33))
    * List()
    *
    * >>> TESTIntCode.diagnostic(0, Array(11002,7,3,0,4,0,99,33))
    * List(99)
   **/
  def diagnostic(input: Int, xs: Array[Int]) = {
    run(Config(input, 0, xs, List())).reverse
  }
}
