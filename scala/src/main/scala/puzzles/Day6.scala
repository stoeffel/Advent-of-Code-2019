package puzzles
import cats.effect._

object Day6 extends AoCApp {
  def run()(implicit cs: ContextShift[IO]) =
    for {
      lines <- Util
        .readFile("../input/day-6.txt")
        .map(_.getLines)
        .map(_.toList)
      orbit <- IO { Orbit.fromList(lines) }
      result1 <- IO { orbit.count }
      result2 <- IO { orbit.countFromTo("YOU", "SAN") }
      _ <- Util.printStrLn(s"Part 1: $result1")
      _ <- Util.printStrLn(s"Part 2: $result2")
    } yield ()
}
