package puzzles
import cats.effect._

object Day4 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      part1 <- IO(Password.possibilities(Password.hasDouble1, _, _))
      part2 <- IO(Password.possibilities(Password.hasDouble2, _, _))

      result1 <- IO(part1(240920, 789857))
      result2 <- IO(part2(240920, 789857))
      _ <- Util.printStrLn(s"AoC 19 - Day 4 Part 1: $result1")
      _ <- Util.printStrLn(s"AoC 19 - Day 4 Part 2: $result2")
    } yield ExitCode.Success
}
