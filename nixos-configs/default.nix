{ flake
, nixpkgs
}:

{
  hendrix = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules = [
      ./hendrix/default.nix
    ];
    specialArgs.flake = flake;
  };
}
