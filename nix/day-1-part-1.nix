{ input, pkgs ? import <nixpkgs> { }, _ ? import ./_.nix { inherit pkgs; } }:
with pkgs;
let calcFuel = acc: mass: acc + ((_.safeToInt mass) / 3 - 2);
in _.compose (lib.foldl' calcFuel 0) _.file.readLines input
