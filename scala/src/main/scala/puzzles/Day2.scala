package puzzles
import cats.effect._

object Day2 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      _ <- BasicIntCode
        .parse("../input/day-2.txt")
        .map(
          BasicIntCode.restoreGravityAssist(_, BasicIntCode.NounVerb.default)
        )
        .map(BasicIntCode.run(pos = 0, _))
        .flatMap(r => Util.printStrLn(s"Part 1: $r"))
        .map(_ => ExitCode.Success)
      _ <- BasicIntCode
        .parse("../input/day-2.txt")
        .map(BasicIntCode.determinePair(goal = 19690720, _))
        .flatMap(r => Util.printStrLn(s"Part 2: $r"))
        .map(_ => ExitCode.Success)
    } yield ()
}
