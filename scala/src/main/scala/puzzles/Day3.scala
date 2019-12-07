package puzzles

object Day3Part1 extends App {
  Wire
    .parse("../input/day-3.txt")
    .map(Wire.closestIntersection)
    .flatMap(r => Util.printStrLn(s"AoC 19 - Day 3: $r"))
    .unsafeRunSync
}

object Day3Part2 extends App {
  Wire
    .parse("../input/day-3.txt")
    .map(Wire.leastSteps)
    .flatMap(r => Util.printStrLn(s"AoC 19 - Day 3: $r"))
    .unsafeRunSync
}
