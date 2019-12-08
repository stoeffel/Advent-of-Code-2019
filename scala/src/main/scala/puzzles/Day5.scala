package puzzles
import cats.effect.IO

object Day5 extends App {
  (for {
    code <- TESTIntCode.parse("../input/day-5.txt")
    _ <- TESTIntCode.diagnostic(
      Util
        .readLineIO("What is the diagnostic code for system ID 5? ")
        .map(_.toInt),
      result => Util.printStrLn(s"AoC 19 - Day 5: $result"),
      code
    )
  } yield ()).unsafeRunSync
}
