package puzzles
import cats.effect._

object Day3 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      _ <- Wire
        .parse("../input/day-3.txt")
        .map(Wire.closestIntersection)
        .flatMap(r => Util.printStrLn(s"Part 1: $r"))
        .map(_ => ExitCode.Success)
      _ <- Wire
        .parse("../input/day-3.txt")
        .map(Wire.leastSteps)
        .flatMap(r => Util.printStrLn(s"Part 2: $r"))
        .map(_ => ExitCode.Success)
    } yield ()
}
