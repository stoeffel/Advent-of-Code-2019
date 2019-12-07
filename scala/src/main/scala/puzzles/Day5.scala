package puzzles
import cats.effect.IO

object Day5 extends App {
  (for {
    code <- TESTIntCode.parse("../input/day-5.txt")
    result <- TESTIntCode.diagnostic(
      Util
        .readLineIO("What is the diagnostic code for system ID 5? ")
        .map(_.toInt),
      code
    )
    _ <- Util.printStrLn(s"AoC 19 - Day 5: $result")
  } yield ()).unsafeRunSync
}
