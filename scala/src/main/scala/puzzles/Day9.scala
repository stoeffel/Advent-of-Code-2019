package puzzles
import cats.effect._
import cats.syntax.all._

object Day9 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      test <- IntCode.parse("../input/day-9-test1.txt")
      testResult <- IntCode.run(IO(0), test)
      _ <- Util.printStrLn(s"Test: $testResult")
      code <- IntCode.parse("../input/day-9.txt")
      result1 <- IntCode.run(IO(1), code)
      code <- IntCode.parse("../input/day-9.txt")
      result2 <- IntCode.run(IO(2), code)
      _ <- Util.printStrLn(s"Part 1: $result1")
      _ <- Util.printStrLn(s"Part 2: $result2")
    } yield ()
}
