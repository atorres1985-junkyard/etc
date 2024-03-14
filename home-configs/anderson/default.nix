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

    bashmount = {
      enable = true;
      extraConfig = readFileFromPath {
        name = "bashmount.sh";
        path = ./dotfiles/bashmount.sh;
      };
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

    rtorrent = {
      enable = true;
      extraConfig = readFileFromPath {
        name = "rtorrent.rc";
        path = dotfiles/rtorrent.rc;
      };
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

    # How to find Elisp packages:
    # nix-env -f "<nixpkgs>" -qaP -A emacsPackages
    # nix search nixpkgs emacsPackages
    emacs = {
      enable = true;
      package = pkgs.emacs29-pgtk.override { };
      extraPackages = epkgs: (builtins.attrValues {
        inherit (epkgs)
          # alsamixer
          # basic-theme # buggy
          # bnf-mode - failing
          # cask - failing
          # cask-mode - failing
          # cmake-mode
          # company-auctex
          # counsel
          # direnv
          # ewal-spacemacs-themes
          # flx-ido
          # helm
          # helm-dash
          # helm-flx
          # helm-rg
          # ivy
          # ivy-historian
          # leaf
          # leaf-convert
          # leaf-keywords
          # leaf-manager
          # leaf-tree
          # melancholy-theme # broken?
          # mpv
          # multishell
          # projectile-direnv # broken
          # timu-spacegrey-theme # broken hash
          # undo-fu
          # use-package
          # w3
          # yasnippet
          # yasnippet-snippets
          # ztree

          abyss-theme
          ace-window
          acme-theme
          adoc-mode
          afternoon-theme
          aggressive-fill-paragraph
          aggressive-indent
          ahungry-theme
          airline-themes
          alect-themes
          all-the-icons
          all-the-icons-dired
          almost-mono-themes
          altcaps
          amx
          anti-zenburn-theme
          auto-complete
          avk-emacs-themes
          avy
          badger-theme
          bar-cursor
          base16-theme
          beacon
          beframe
          beginend
          better-shell
          bind-chord
          bind-key
          bind-map
          birds-of-paradise-plus-theme
          bison-mode
          blackout
          bongo
          borland-blue-theme
          brainfuck-mode
          brutalist-theme
          buffer-move
          bug-hunter
          caroline-theme
          centaur-tabs
          cherry-blossom-theme
          chyla-theme
          cloud-theme
          color-theme
          color-theme-approximate
          color-theme-buffer-local
          color-theme-modern
          color-theme-sanityinc-solarized
          color-theme-sanityinc-tomorrow
          color-theme-x
          colorThemeSolarized
          colorless-themes
          company
          company-lua
          company-math
          company-quickhelp
          company-shell
          consult
          corfu
          crux
          ctrlf
          cyberpunk-theme
          darcula-theme
          dash
          dashboard
          deadgrep
          delight
          denote
          diffview
          diminish
          dimmer
          dired-preview
          dired-sidebar
          diredfl
          dirvish
          disable-mouse
          doom-modeline
          doom-themes
          dracula-theme
          dwim-shell-command
          dynamic-ruler
          eat
          editorconfig
          edwina
          ef-themes
          eink-theme
          elscreen
          elvish-mode
          embark
          embark-consult
          ement
          emms
          envrc
          esup
          exotica-theme
          expand-line
          expand-region
          eyebrowse
          faff-theme
          fancy-dabbrev
          fill-column-indicator
          fish-mode
          flx
          flycheck
          fontaine
          forge
          forth-mode
          frame-mode
          gcmh
          general
          gnu-apl-mode
          god-mode
          grandshell-theme
          hardcore-mode
          haskell-mode
          helpful
          hemisu-theme
          highlight-parentheses
          historian
          htmlize
          humanoid-themes
          hungry-delete
          icomplete-vertical
          json-mode
          keypression
          leuven-theme
          lin
          lua-mode
          magit
          marginalia
          markdown-mode
          material-theme
          meghanada
          meow
          meson-mode
          mini-frame
          minimal-theme
          minimap
          modalka
          modus-themes
          monokai-theme
          multi-vterm
          multiple-cursors
          names
          neotree
          nhexl-mode
          ninja-mode
          nix-mode
          no-littering
          nofrils-acme-theme
          nord-theme
          nordic-night-theme
          number-lock
          nyan-mode
          orderless
          org-contrib
          org-journal
          org-superstar
          origami
          outshine
          page-break-lines
          paper-theme
          paperless
          paredit
          paredit-menu
          pdf-tools
          plan9-theme
          powerline
          prescient
          prism
          projectile
          pulsar
          qtcreator-theme
          rainbow-delimiters
          rg
          rmsbolt
          rust-mode
          ryo-modal
          selected
          setup
          shell-command-plus
          shx
          smart-mode-line
          smartparens
          solarized-theme
          soothe-theme
          spacebar
          spaceline
          spaceline-all-the-icons
          spacemacs-theme
          sr-speedbar
          standard-themes
          substitute
          sunrise-commander
          super-save
          switch-window
          tao-theme
          telephone-line
          tempel
          termbright-theme
          theme-buffet
          timu-caribbean-theme
          timu-macos-theme
          timu-rouge-theme
          toml-mode
          tommyh-theme
          toxi-theme
          transient
          treemacs
          tuareg
          twilight-theme
          ubuntu-theme
          undo-tree
          unfill
          vdiff
          vertico
          visual-fill-column
          vs-dark-theme
          vs-light-theme
          vterm
          vterm-toggle
          vundo
          waher-theme
          wanderlust
          warm-night-theme
          weblorg
          weyland-yutani-theme
          which-key
          window-purpose
          yafolding
          yaml-mode
          yankpad
          yoshi-theme
          zeno-theme
          zig-mode
          zweilight-theme
        ;
      });
      overrides = final: prev: { };
    };
  };

}
