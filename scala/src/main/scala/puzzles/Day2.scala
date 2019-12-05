package puzzles

object Day2Part1 extends App {
  IntCode
    .parse("../input/day-2.txt")
    .map(IntCode.restoreGravityAssist(_, IntCode.NounVerb.default))
    .map(IntCode.run(pos = 0, _))
    .flatMap(r => Util.printStrLn(s"AoC 19 - Day 2: $r"))
    .unsafeRunSync
}

object Day2Part2 extends App {
  IntCode
    .parse("../input/day-2.txt")
    .map(IntCode.determinePair(goal = 19690720, _))
    .flatMap(r => Util.printStrLn(s"AoC 19 - Day 2: $r"))
    .unsafeRunSync
}
