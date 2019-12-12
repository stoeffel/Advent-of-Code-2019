package puzzles
import cats.effect._

object Day5 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      code <- IntCode.parse("../input/day-5.txt")
      result <- IntCode
        .diagnostic(
          Util
            .readLineIO("What is the diagnostic code for system ID 5? ")
            .map(_.toInt),
          code
        )
      _ <- Util.printStrLn(s"Result: $result")
    } yield ()
}
