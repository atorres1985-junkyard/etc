{
  description = "Configuração de Anderson Torres, em flakes";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs: {
    devShells = import ./dev-shells {
      inherit (inputs) nixpkgs;
      flake = inputs.self;
    };
  };
}
