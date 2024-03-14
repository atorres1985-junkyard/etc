{ lib
, pkgs
, ...
}:

let
  readFileFromPath = pathArgs:
    builtins.readFile (builtins.path pathArgs);
in
{
  programs.home-manager.enable = true;

  services = {
    xscreensaver = {
      enable = false;
      settings = {};
    };
  };

  home = {
    stateVersion = "22.11";
    username = "anderson";
    homeDirectory = "/home/anderson";

    enableNixpkgsReleaseCheck = true;

    packages =
      (builtins.attrValues {
        inherit (pkgs.aspellDicts) pt_BR;
      # inherit (pkgs.wayfirePlugins) wcm;
        inherit (pkgs)
          # hypr
          arcan-all-wrapped
          aspell
          bc
          cksfv
          djview
          ed
          elinks
          evince
          fd
          file
          gimp
          git-filter-repo
          glpaper
          gnuplot_qt
          graphicsmagick-imagemagick-compat
          grc
          grim
          hyprland
          labwc
          lxterminal
          lzip
          mc
          mktorrent
          moreutils
          ncftp
          pavucontrol
          pinentry-curses
          poppler_utils
          procs
          pv
          pwvucontrol
          rhash
          rlwrap
          rootbar
          rsclock
          rsync
          screen
          silver-searcher
          slurp
          sword
          tree
          tty-clock
          ucs-fonts
          unar
          unzip
          waybar
          wayfire
          wayland-utils
          wbg
          weston
          wev
          wf-config
          which
          with-shell
          wl-clipboard
          wlr-randr
          wofi
          xchm
          xwayland
          yambar
          yt-dlp
          z-lua
          zathura
          zeal
          zip
        ;
      });
  };

  programs = {
    alacritty = {
      enable = false;
      settings = readFileFromPath {
        name = "alacritty.yml";
        path = ./dotfiles/alacritty.yml;
      };
    };

    bash = {
      enable = true;
      enableVteIntegration = true;
    };

    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        tabs = "4";
        # theme = "Dracula";
      };
      # themes = {};
    };

    broot = {
      enable = true;
      enableBashIntegration = true;
    };

    fish.enable = true;

    gh = {
      enable = true;
      extensions = [];
      settings.prompt = "enabled";
    };

    git = {
      enable = true;
      package = pkgs.git;
      lfs.enable = true;
      diff-so-fancy.enable = true;
      userName = "Anderson Torres";
      userEmail = "torres.anderson.85@protonmail.com";
      aliases = {
        rb = "rebase";
      };
    };

    # nix-index.enable = true;
    # nix-index-database.comma.enable = true;

    ripgrep = {
      enable = true;
      arguments = [];
    };

    bottom.enable = true;
    brave.enable = true;
    direnv.enable = false;
    eza.enable = true;
    firefox.enable = true;
    foot.enable = true;
    fzf.enable = true;
    gpg.enable = true;
    hexchat.enable = true;
    jq.enable = true;
    kitty.enable = true;
    mpv.enable = true;
    pandoc.enable = true;
  };

}
