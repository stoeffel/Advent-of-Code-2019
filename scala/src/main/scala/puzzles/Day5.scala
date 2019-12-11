package puzzles
import cats.effect._

object Day5 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      code <- IntCode.parse("../input/day-5.txt")
      result <- IntCode.diagnostic(
        Util
          .readLineIO("What is the diagnostic code for system ID 5? ")
          .map(_.toInt),
        code
      )
      _ <- Util.printStrLn(s"AoC 19 - Day 5: $result")
    } yield ExitCode.Success
}
