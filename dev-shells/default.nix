{ nixpkgs
, system ? "x86_64-linux"
, pkgs ? nixpkgs.legacyPackages."${system}"
, lib ? pkgs.lib
, flake
, ...
}:

{
}
