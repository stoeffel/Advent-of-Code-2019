{ pkgs ? import <nixpkgs> { }, _ ? import ./_.nix { inherit pkgs; } }:
with builtins;
with pkgs; rec {
  sortString = _.composing [
    lib.concatStrings
    (map toString)
    (lib.sort lib.lessThan)
    (map lib.toInt)
    lib.stringToCharacters
  ];

  increasingNumbers = x: x == sortString x;

  groupChars = _.compose _.groupWhileEq lib.stringToCharacters;

  hasDouble = _.compose (any (x: length x >= 2)) groupChars;

  exactly2 = _.compose (any (x: length x == 2)) groupChars;

  possibilities = pred: from: to:
    _.compose (lib.count (_.const true)) (filter (_.compose pred toString))
    (lib.range from to);

  validate1 = x: increasingNumbers x && hasDouble x;
  validate2 = x: increasingNumbers x && exactly2 x;

  part1 = possibilities validate1;
  part2 = possibilities validate2;

  inputA = 240920;
  inputB = 789857;
}
