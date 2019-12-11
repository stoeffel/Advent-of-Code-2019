package puzzles
import cats.effect._

object Day2Part1 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    BasicIntCode
      .parse("../input/day-2.txt")
      .map(BasicIntCode.restoreGravityAssist(_, BasicIntCode.NounVerb.default))
      .map(BasicIntCode.run(pos = 0, _))
      .flatMap(r => Util.printStrLn(s"AoC 19 - Day 2: $r"))
      .map(_ => ExitCode.Success)
}

object Day2Part2 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    BasicIntCode
      .parse("../input/day-2.txt")
      .map(BasicIntCode.determinePair(goal = 19690720, _))
      .flatMap(r => Util.printStrLn(s"AoC 19 - Day 2: $r"))
      .map(_ => ExitCode.Success)
}
