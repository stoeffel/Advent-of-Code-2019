{ pkgs }:
with builtins;
with pkgs.lib; rec {
  identity = x: x;
  compose = f: g: x: f (g x);
  flip = f: x: y: f y x;
  composing = fs: x: foldl compose identity fs x;
  sum = foldl' (acc: x: acc + x) 0;
  strings = {
    unlines = concatStringsSep "\n";
    unwords = concatStringsSep " ";
  };
  file = {
    readLines = compose (splitString "\n") fileContents;
    readJSON = compose fromJSON readFile;
  };
  withDefault = x: { success, value }: if success then value else x;
  safeToInt = composing [ (withDefault 0) tryEval toInt ];
}
