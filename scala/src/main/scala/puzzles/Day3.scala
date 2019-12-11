package puzzles
import cats.effect._

object Day3Part1 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Wire
      .parse("../input/day-3.txt")
      .map(Wire.closestIntersection)
      .flatMap(r => Util.printStrLn(s"AoC 19 - Day 3: $r"))
      .map(_ => ExitCode.Success)
}

object Day3Part2 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    Wire
      .parse("../input/day-3.txt")
      .map(Wire.leastSteps)
      .flatMap(r => Util.printStrLn(s"AoC 19 - Day 3: $r"))
      .map(_ => ExitCode.Success)
}
