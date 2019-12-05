package puzzles
import cats.effect.IO

object Day5 extends App {
  (for {
    code <- TESTIntCode.parse("../input/day-5.txt")
    input <- Util.readLineIO("What is the diagnostic code for system ID 5? ")
    result <- IO { TESTIntCode.diagnostic(input.toInt, code) }
    _ <- Util.printStrLn(s"AoC 19 - Day 2: $result")
  } yield ()).unsafeRunSync
}
