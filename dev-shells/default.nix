{ nixpkgs
, system ? "x86_64-linux"
, pkgs ? nixpkgs.legacyPackages."${system}"
, lib ? pkgs.lib
, flake
, ...
}:

{
  gaming-emulation = pkgs.mkShell {
    buildInputs = builtins.attrValues {
      inherit (pkgs)
        ares
        desmume
        duckstation
        higan
        mgba
        openmsx
        pcsx2
        rpcs3
        stella
      ;
    };

    shellHook = ''
      PS1="[\u@\h:\w]\ngaming-emulation> "
    '';
  };

  nix-tools = pkgs.mkShell {
    buildInputs = builtins.attrValues {
      inherit (pkgs)
        nix-diff
        nix-doc
        nix-du
        nix-index
        nix-prefetch
        nix-prefetch-scripts
        nix-top
        nix-tour
        nix-universal-prefetch
        nix-update
        nixos-shell
      ;
    };

    shellHook = ''
      PS1="[\u@\h:\w]\nnix-tools> "
    '';
  };

  office-shell = pkgs.mkShell {
    buildInputs = lib.attrVals [
      "gnumeric"
      "libreoffice-qt"
    ] pkgs;

    shellHook = ''
      PS1="[\u@\h:\w]\noffice-shell> "
    '';
  };

  texlive-shell = pkgs.mkShell {
    buildInputs = builtins.attrValues {
      inherit (pkgs.texlive.combined)
        scheme-full
      ;
    };
  };

  vcs-shell = pkgs.mkShell {
    buildInputs = builtins.attrValues {
      inherit (pkgs)
        cvs2svn
        gitFull
        nano
        reposurgeon
        svn2git
        subversion
        mercurialFull
      ;
    };

    shellHook = ''
      PS1="[\u@\h:\w]\nvcs-shell> "
    '';
  };

  ventoy-shell = pkgs.mkShell {
    buildInputs = lib.attrVals [
      "ventoy"
    ] pkgs;

    shellHook = ''
      PS1="[\u@\h:\w]\nventoy-shell> "
    '';
  };
}
