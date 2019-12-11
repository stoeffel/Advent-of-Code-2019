package puzzles
import cats.effect._

object Day6 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      lines <- Util
        .readFile("../input/day-6.txt")
        .map(_.getLines)
        .map(_.toList)
      orbit <- IO { Orbit.fromList(lines) }
      result1 <- IO { orbit.count }
      result2 <- IO { orbit.countFromTo("YOU", "SAN") }
      _ <- Util.printStrLn(s"AoC 19 - Day 6: $result1")
      _ <- Util.printStrLn(s"AoC 19 - Day 6: $result2")
    } yield ExitCode.Success
}
