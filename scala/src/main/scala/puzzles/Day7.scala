package puzzles
import cats.effect.IO

object Day7 extends App {
  (for {
    code <- TESTIntCode.parse("../input/day-7.txt")
    result1 <- Amp.maxThrusterSignal(0, 4, code)
    // result2 <- IO { orbit.countFromTo("YOU", "SAN") }
    _ <- Util.printStrLn(s"AoC 19 - Day 7: $result1")
    // _ <- Util.printStrLn(s"AoC 19 - Day 7: $result2")
  } yield ()).unsafeRunSync
}
