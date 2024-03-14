{ lib
, config
, flake
, modulesPath
, options
, pkgs
, ...
}:

{
  imports = [
    ./hardware-configuration.nix
  ];

  boot = {
    readOnlyNixStore = true;

    kernel.sysctl."kernel.sysrq" = 1;

    kernelParams = [ ];

    loader = {
      timeout = 60;
      efi.canTouchEfiVariables = false;

      systemd-boot.enable = false;

      grub = {
        enable = true;
        forceInstall = false;

        copyKernels = true;
        devices = [ "nodev" ];
        efiInstallAsRemovable = true;
        efiSupport = true;
        fsIdentifier = "label";
        # splashImage = "/etc/nixos/share/boot-backgrounds/dummy.png";
        splashMode = "stretch";

        backgroundColor = "#000000";

        extraEntries = ''
          menuentry "Reboot" {
            reboot
          }
          menuentry "Poweroff" {
            halt
          }
        '';
      };
    };
  };

  console = {
    font = "LatGrkCyr-12x22";
    useXkbConfig = true;
  };

  time.timeZone = "America/Sao_Paulo";

  fonts = {
    fontDir.enable = true;
    enableDefaultPackages = true;
    fontconfig = {
      enable = true;
      hinting.enable = true;
      includeUserConf = true;
    };

    packages = [
      pkgs.mplus-outline-fonts.githubRelease
      (pkgs.nerdfonts.override {
        fonts = [
          "CodeNewRoman"
          "DejaVuSansMono"
          "DroidSansMono"
          "FiraCode"
          "SpaceMono"
        ];
      })
    ] ++ (builtins.attrValues {
      inherit (pkgs)
        anonymousPro
        dina-font
        emacs-all-the-icons-fonts
        fantasque-sans-mono
        fira-code
        fira-code-symbols
        hack-font
        junicode
        liberation_ttf
        monoid
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        profont
        proggyfonts
        recursive
        roboto
        roboto-mono
        roboto-slab
      ;
    });
  };

  services.kmscon = {
    enable = false;
    hwRender = true;
    extraConfig = ''
      # font-name = Inconsolata
      font-size = 14
      term = xterm-256color
      sb-size = 2500
      xkb-layout = us
      xkb-variant = intl
      xkb-model = abnt
      xkb-repeat-delay = 250
      xkb-repeat-rate = 50
      drm = on
    '';
  };

  networking = {
    hostName = "hendrix";
    firewall.enable = true;
  };

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man.enable = true;
    nixos.enable = true;
  };

  nix = {
    package = pkgs.nixUnstable;
    settings = {
      cores = 6;
      extra-sandbox-paths = [ "/dev" "/proc" "/sys" ];
      max-jobs = 4; # pkgs.lib.mkForce 4;
      sandbox = true; # pkgs.lib.mkForce true;
      substituters = [ ];

      # From extraOptions
      binary-caches-parallel-connections = 10;
      build-keep-log = true;
      build-use-substitutes = true;
      experimental-features = [ "nix-command" "flakes" ];
      gc-keep-derivations = true;
      gc-keep-outputs = true;
    };
    # To reduce disk usage
    daemonIOSchedClass = "best-effort";
    daemonIOSchedPriority = 7;
    daemonCPUSchedPolicy = "batch";

    nrBuildUsers = 10;

    sshServe.enable = false;

    gc.automatic = false;
    gc.dates = "12:00";

    # Set the nixpkgs entry for $NIX_PATH. Without this, commands like
    # `nix-shell -p pkgs.higan` will use an outdated nixpkgs version
    # nixPath = [
    #   "nixpkgs=${flake.inputs.nixpkgs}"
    # ];
    # The above, but for Nix3 commands like `nix shell nixpkgs#higan`
    # registry.nixpkgs.flake = flake;
  };

  nixpkgs = {
    overlays = [ ];
  };

  hardware.opengl = {
    enable = true;
    extraPackages = [ ]; # Trocar em breve!
    driSupport = true;
  };

  # pulseaudio conflict with pipewire
  hardware.pulseaudio.enable = !(config.services.pipewire.enable);

  # rtkit is optional for pipewire, but recommended
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;

    #media-session.enable = true;

    # config.pipewire was disabled because it never worked seriously
  };

  security = {
    sudo = {
      enable = false;
      wheelNeedsPassword = false;
    };

    doas = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  # In my heart, NetworkManager is a service, not a subkey of networking
  # However, it is hard to change it on NixOS - why?
  networking.networkmanager = {
    enable = false;
    enableStrongSwan = false;
  };

  environment.etc."wpa_supplicant.conf".text =
    lib.mkIf config.services.connman.enable ''
      # dummy file to make connman happy with wpa_supplicant
    '';

  services = {
    connman = {
      enable = true;
      enableVPN = true;
      package = pkgs.connmanFull;
      wifi.backend = "wpa_supplicant";
    };

    gpm = {
      enable = false;
      protocol = "imps/2";
    };

    ntp.enable = false;
    openssh.enable = false;
    udisks2.enable = true;
    upower.enable = true;

    printing = {
      enable = false;
      tempDir = "/tmp/cups";
    };
  };

  users = {
    mutableUsers = true;
    enforceIdUniqueness = true;
    defaultUserShell = "/run/current-system/sw/bin/bash";

    users."visita" = {
      description = "Visita";
      isNormalUser = true;
      password = "";
    };
  };


  virtualisation = {
    virtualbox = {
      guest.enable = false;
      host = {
        enable = true;
        enableHardening = true;
        addNetworkInterface = true;
        headless = false;
      };
    };

    libvirtd = {
      enable = false;
      qemu.package = pkgs.qemu_kvm;
    };
  };

  services.picom = {
    enable = false;
    activeOpacity = 0.9;
    fade = true;
    inactiveOpacity = 0.5;
    shadow = true;
    shadowOffsets = [ (-10) (-10) ];
  };

  services.xserver = {
    enable = true;
    autorun = true;

    enableCtrlAltBackspace = true;
    layout = "us";
    # xkbOptions = "eurosign:e";
    xkbModel = "pc105";
    xkbVariant = "intl";
    exportConfiguration = true;
    videoDrivers =  [
      "amdgpu"
      # "ati"
      # "radeon"
      # "vesa"
      # "cirrus"
      # "modesetting"
      # "fbdev"
    ];

    # Touchpad
    libinput = {
      enable = true; # incompatible with synaptics
      mouse = {
        accelProfile = "adaptive";
        accelSpeed = "0.050";
        buttonMapping = "1 2 3";
        disableWhileTyping = true;
      };
    };

    synaptics = {
      enable = false; # incompatible with libinput
      accelFactor = "0.050";
      buttonsMap = [ 1 2 3 ];
      fingersMap = [ 1 3 2 ];
      horizontalScroll = true;
      minSpeed = "0.5";
      maxSpeed = "1.5";
      tapButtons = true;
      twoFingerScroll = true;
      vertEdgeScroll = true;
      palmDetect = true;
    };

    displayManager = {
      gdm.enable = false;
      lightdm.enable = true;
      sddm.enable = false;
      startx.enable = false;
      sx.enable = false;
      xpra.enable = false;

      xserverArgs = [
        "-ac"
        "-logverbose"
        "-nolisten tcp"
        "-verbose"
      ];
    };

    desktopManager = {
      # runXdgAutostartIfNone.enable = false; #
      # wallpaper?
      budgie.enable = false; # Conflicts Pantheon
      cde.enable = false;
      cinnamon.enable = false;
      deepin.enable = false;
      enlightenment.enable = true;
      gnome.enable = true; # Conflicts budgie
      kodi.enable = false;
      lumina.enable = true;
      lxqt.enable = true;
      mate.enable = true;
      pantheon.enable = false; # Conflicts sddm
      phosh.enable = false;
      plasma5.enable = false; # I am tired
      retroarch.enable = false;
      surf-display.enable = false;
      xfce.enable = true;
      xterm.enable = true;
    };

    windowManager = {
      # cwm.enable = false;
      # jwm.enable = false;
      # matchbox.enable = false;
      # metacity.enable = false;
      # mwm.enable = false;
      # oroborus.enable = false;
      # qtile.enable = false;
      # twm.enable = false;
      # wmii.enable = false;
      evilwm.enable = false;
      fluxbox.enable = true;
      fvwm3.enable = true;
      icewm.enable = true;
      notion.enable = false;
      openbox.enable = true;
      pekwm.enable = true;
      ratpoison.enable = true;
      sawfish.enable = true;
      spectrwm.enable = false;
      stumpwm.enable = false;
      windowlab.enable = false;
      windowmaker.enable = true;
    };
  };

  environment.systemPackages = let
    custom-emacs = ((pkgs.emacsPackagesFor pkgs.emacs29-nox).emacsWithPackages)
      (epkgs: [
        # Useful for editing Nix files! :)
        epkgs.melpaPackages.nix-mode
      ]);
  in
    [ custom-emacs ] ++ (builtins.attrValues {
      inherit (pkgs)

        # xorg.xdpyinfo xcompmgr
        # home-manager
        curl

        dmidecode
        elinks
        efibootmgr
        exfat
        f2fs-tools
        fontconfig
        gitMinimal
        gparted
        jfsutils
        micro
        nix-diff
        nix-du
        nix-index
        nix-prefetch
        nix-prefetch-scripts
        nix-tour
        nix-universal-prefetch
        pciutils
        rsync zsync
        wget
        xfsprogs
      ;
    });

  swapDevices = lib.mkOverride 5 [
    { device = "/0-swapfiles/0.swap";
      size = 12301;
    }
    { device = "/0-swapfiles/1.swap";
      size = 12301;
    }
  ];

  system.nixos.tags = [
    "hendrix"
    "connman"
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "24.05"; # Did you read the comment?
}
