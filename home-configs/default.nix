{ home-manager
, flake
, nixpkgs
}:

{
  anderson = home-manager.lib.homeManagerConfiguration {
    pkgs = import nixpkgs {
      system = "x86_64-linux";
      overlays = [
      ];
    };
    modules = [
      ./anderson/default.nix
    ];
    extraSpecialArgs.flake = flake;
  };
}
