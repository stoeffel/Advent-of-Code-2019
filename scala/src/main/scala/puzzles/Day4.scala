package puzzles
import cats._
import cats.implicits._

object Day4 extends App {
  val part1 = Password.possibilities(Password.hasDouble1) _
  val part2 = Password.possibilities(Password.hasDouble2) _

  val result1 = part1(240920, 789857)
  val result2 = part2(240920, 789857)
  Util.printStrLn(s"AoC 19 - Day 2 Part 1: $result1").unsafeRunSync
  Util.printStrLn(s"AoC 19 - Day 2 Part 2: $result2").unsafeRunSync
}
