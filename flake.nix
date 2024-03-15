{
  description = "Configuração de Anderson Torres, em flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: {
    devShells = import ./dev-shells {
      inherit (inputs) nixpkgs;
      flake = inputs.self;
    };

    homeConfigurations = import ./home-configs {
      inherit (inputs) home-manager nixpkgs;
      flake = inputs.self;
    };

    nixosConfigurations = import ./nixos-configs {
      inherit (inputs) nixpkgs;
      flake = inputs.self;
    };
  };
}
