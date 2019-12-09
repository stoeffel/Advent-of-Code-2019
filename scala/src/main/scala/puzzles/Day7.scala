package puzzles
import cats.effect._
import cats.syntax.all._

object Day7 extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      code <- IntCode.parse("../input/day-7.txt")
      result1 <- Amp.maxThrusterSignal(0, 4, Amp.Normal(), code)
      result2 <- Amp.maxThrusterSignal(5, 9, Amp.FeedbackLook(), code)
      _ <- Util.printStrLn(s"AoC 19 - Day 7: $result1")
      _ <- Util.printStrLn(s"AoC 19 - Day 7: $result2")
    } yield (ExitCode.Success)
}
