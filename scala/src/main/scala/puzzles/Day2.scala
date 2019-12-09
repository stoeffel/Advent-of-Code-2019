package puzzles

object Day2Part1 extends App {
  BasicIntCode
    .parse("../input/day-2.txt")
    .map(BasicIntCode.restoreGravityAssist(_, BasicIntCode.NounVerb.default))
    .map(BasicIntCode.run(pos = 0, _))
    .flatMap(r => Util.printStrLn(s"AoC 19 - Day 2: $r"))
    .unsafeRunSync
}

object Day2Part2 extends App {
  BasicIntCode
    .parse("../input/day-2.txt")
    .map(BasicIntCode.determinePair(goal = 19690720, _))
    .flatMap(r => Util.printStrLn(s"AoC 19 - Day 2: $r"))
    .unsafeRunSync
}
