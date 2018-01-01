{ config, pkgs, ... }:

let
  nixpkgs-bootstrap = import <nixpkgs> { };
  nixpkgs-16_09 = import (nixpkgs-bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "b833b10f81bee30c634802afbe06d9d0630c2709";
    sha256 = "01sdf85fb99vmfk2xnjm7ii7ac0vf112ywhkmi1v5dln1x2cxps7";
  }) { };
in
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  system.stateVersion = "16.09";

  nixpkgs.config.allowUnfree = true;

  nix = {
    binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
    binaryCaches = [ "https://cache.nixos.org" "https://nixcache.reflex-frp.org" ];

    nixPath =
      let dotfiles = "/home/drets/src/dots";
      in [
        "nixos-config=${dotfiles}/nixos/configuration.nix"
        "${dotfiles}/channels"
      ];

    useSandbox = true;
  };

  hardware = {
    bluetooth.enable = false;
    enableAllFirmware = true;
    facetimehd.enable = true;
    opengl.driSupport32Bit = true;
    pulseaudio = {
      enable = true;
      support32Bit = true;
    };
  };

  powerManagement.enable = true;

  boot = {
    blacklistedKernelModules = ["bdc_pci"];
    kernelParams = [ "hid_apple.iso_layout=0" "systemd.legacy_systemd_cgroup_controller=yes" ];

    initrd.luks.devices = [{
      name = "rootfs";
      device = "/dev/sda3";
      preLVM = true;
    }];

    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      timeout = 0;
    };
  };

  networking = {
    hostName = "MBP";
    useDHCP = false;
    wicd.enable = true;
    wireless.enable = false;
    firewall = {
      allowedTCPPorts = [
        5000
      ];
    };
  };

  services = {
    udev.packages = [ pkgs.android-udev-rules ];

    dnsmasq = {
      enable = true;

      # These are used in addition to resolv.conf
      servers = [ "8.8.8.8" "8.8.4.4" ];

      extraConfig = ''
        listen-address=127.0.0.1
        cache-size=5000

        no-negcache
      '';
    };

    redis.enable = true;

    locate = {
      enable = true;
      localuser = "drets";
      interval = "*-*-* 17:00:00";
    };

    xserver = {
      enable = true;
      synaptics = {
        enable = true;
        twoFingerScroll = true;
        vertEdgeScroll = true;
        accelFactor = "0.1";
        maxSpeed = "1.5";
        buttonsMap = [ 1 3 2 ];
        palmDetect = true;
      };
      windowManager.awesome = {
        enable = true;
        luaModules = [ pkgs.luaPackages.luafilesystem pkgs.luaPackages.cjson ];
      };
    };

    redshift = {
      enable = true;
      latitude = "49.55";
      longitude = "25.59";
    };

    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandlePowerKey=ignore
    '';

    openssh = {
      enable = true;
      passwordAuthentication = false;
    };

    postgresql = {
      authentication = "local all all ident";
      enable = true;
    };
  };

  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" ];

  time.timeZone = "Europe/Amsterdam";

  users = {
    extraGroups.plugdev = { };

    extraUsers.drets = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "plugdev" "dialout" "users" "wheel" "dialout" "networkmanager" ];
      initialPassword = "foobar";
    };

    defaultUserShell = pkgs.bash;
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = false;

    fonts = with pkgs; [
      corefonts
      fira-code-symbols
      iosevka
      dejavu_fonts
      inconsolata
      powerline-fonts
      source-code-pro
      terminus_font
      ubuntu_font_family
      unifont
      vistafonts
    ];
  };

  programs.kbdlight.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  systemd = {
    services.healthySleep = {
      script = "shutdown -P";
    };

    timers.healthySleep = {
      partOf = [ "healthySleep.service" ];
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "*-*-* 23:30:00";
    };
  };

  virtualisation.virtualbox.host.enable = true;

  environment = {
    # Put the text in /etc/resolv.conf.head
    #
    # That will prepend dnsmasq server to /etc/resolv.conf (dhcpcd-specific)
    etc."resolv.conf.head".text = ''
      nameserver 127.0.0.1
    '';

    shellInit = ''
      export GTK_PATH=$GTK_PATH:${nixpkgs-16_09.oxygen_gtk}/lib/gtk-2.0
      export GTK2_RC_FILES=$GTK2_RC_FILES:${nixpkgs-16_09.oxygen_gtk}/share/themes/oxygen-gtk/gtk-2.0/gtkrc
    '';
    pathsToLink = [ "/share" ];
    systemPackages = with pkgs; [
      ag
      anki
      aspell
      aspellDicts.en
      aspellDicts.uk
      aspellDicts.ru
      aspellDicts.de
      bc
      # cabal-install
      chromedriver
      deadbeef
      diffutils
      dropbox
      emacs
      emacsPackages.proofgeneral
      epdfview
      file
      firefox
      jq
      gcc
      gimp
      git
      graphicsmagick
      global
      gnome3.adwaita-icon-theme
      gnome3.gconf
      gnumake
      gnupg
      gnuplot
      goldendict
      google-chrome
      # haskellPackages.ghc
      # haskellPackages.hasktags
      # haskellPackages.hlint
      hexchat
      htop
      kde4.kde_baseapps
      keepassx
      libreoffice
      man-pages
      mplayer
      nix-repl
      nox
      # openjdk
      openvpn
      patchelf
      p7zip
      python
      python3
      qbittorrent
      rxvt_unicode
      rsync
      scrot
      shared_mime_info
      shutter
      slack
      stack
      tightvnc
      # texlive.combined.scheme-full
      tdesktop
      unclutter
      unzip
      vlc
      wget
      which
      whois
      xclip
      xorg.xbacklight
      xorg.xkbcomp
      xorg.xwininfo
      xkbset
      xscreensaver
      zip
    ] ++ (with nixpkgs-16_09; [
      oxygen-gtk2
      oxygen-gtk3
    ]);
  };
}
