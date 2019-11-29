{ input, pkgs ? import <nixpkgs> { }, _ ? import ./_.nix { inherit pkgs; } }:
with pkgs;
let
  calcFuel = acc: mass: acc + calcFuel' (_.safeToInt mass);
  calcFuel' = mass:
    let fuel = mass / 3 - 2;
    in if fuel <= 0 then 0 else fuel + calcFuel' fuel;

in _.compose (lib.foldl' calcFuel 0) _.file.readLines input
