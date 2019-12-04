{ pkgs }:
with builtins;
with pkgs.lib; rec {
  const = x: y: x;
  identity = x: x;
  compose = f: g: x: f (g x);
  flip = f: x: y: f y x;
  composing = fs: x: foldl compose identity fs x;
  sum = foldl' (acc: x: acc + x) 0;
  updateAt = i: f: xs: imap0 (index: x: if i == index then f x else x) xs;
  setAt = i: x: xs: updateAt i (_: x) xs;
  strings = {
    unlines = concatStringsSep "\n";
    unwords = concatStringsSep " ";
  };
  file = {
    readLines = compose (splitString "\n") fileContents;
    readJSON = compose fromJSON readFile;
    readCSV = compose (splitString ",") fileContents;
  };
  withDefault = x: { success, value }: if success then value else x;
  safeToInt = composing [ (withDefault 0) tryEval toInt ];
  groupWhileEq = foldr (x: acc:
    if acc == [ ] then
      [ [ x ] ]
    else if x == head (head acc) then
      [ ([ x ] ++ (head acc)) ] ++ (tail acc)
    else
      [ [ x ] ] ++ acc) [ ];
}
