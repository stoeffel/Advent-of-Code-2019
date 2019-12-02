{ input, pkgs ? import <nixpkgs> { }, _ ? import ./_.nix { inherit pkgs; } }:
with builtins;
with pkgs; rec {
  initialIntcode = _.compose (map _.safeToInt) _.file.readCSV input;
  restoreGravityAssist = { noun, verb }:
    _.compose (_.setAt 2 verb) (_.setAt 1 noun);
  add = a: b: a + b;
  mul = a: b: a * b;
  deref = pos: xs: elemAt xs (elemAt xs pos);
  setOut = pos: x: xs: _.setAt (elemAt xs pos) x xs;
  binOp = f: xs: pos:
    let result = f (deref pos xs) (deref (pos + 1) xs);
    in runProgram (pos + 3) (setOut (pos + 2) result xs);
  runProgram = pos: xs:
    if pos >= length xs then
      xs
    else
      let curr = elemAt xs pos;
      in if curr == 1 then
        binOp add xs (pos + 1)
      else if curr == 2 then
        binOp mul xs (pos + 1)
      else if curr == 99 then
        xs
      else
        throw "unknown op";

  runIntcode = nounVerb:
    _.composing [
      (_.flip elemAt 0)
      (runProgram 0)
      (restoreGravityAssist nounVerb)
    ];

  ## ========
  ##  PART 1
  ## ========
  part1 = runIntcode {
    noun = 12;
    verb = 2;
  } initialIntcode;

  ## ========
  ##  PART 2
  ## ========
  goal = 19690720;

  determinePair = { noun, verb }:
    xs:
    if runIntcode { inherit noun verb; } xs == goal then
      100 * noun + verb
    else if verb == 99 then
      determinePair {
        noun = noun + 1;
        verb = 0;
      } initialIntcode
    else
      determinePair {
        inherit noun;
        verb = verb + 1;
      } initialIntcode;

  part2 = determinePair {
    noun = 0;
    verb = 0;
  } initialIntcode;
}
