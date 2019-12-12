package puzzles
import cats.effect._
import cats.syntax.all._

object Day7 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      code <- IntCode.parse("../input/day-7.txt")
      result1 <- Amp.maxThrusterSignal(0, 4, Amp.Normal(), code)
      result2 <- Amp.maxThrusterSignal(5, 9, Amp.FeedbackLook(), code)
      _ <- Util.printStrLn(s"Part 1: $result1")
      _ <- Util.printStrLn(s"Part 2: $result2")
    } yield ()
}
