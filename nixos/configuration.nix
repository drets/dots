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
    bluetooth.enable = true;
    enableAllFirmware = true;
    facetimehd.enable = true;
    opengl.driSupport32Bit = true;
    pulseaudio = {
      enable = true;
      support32Bit = true;
      package = pkgs.pulseaudioFull;
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
    networkmanager.enable = true;
    # disable wpa_supplicant
    wireless.enable = false;
    firewall = {
      allowedTCPPorts = [
        5000
      ];
    };
  };

  services = {
    udev.packages = [ pkgs.android-udev-rules ];

    elasticsearch = {
      enable = true;
      cluster_name = "drets";
      extraConf = ''
        node.name: "MBP"
      '';
    };

    kibana = {
      enable = true;
    };

    logstash = {
      enable = true;
      inputConfig = ''
        file {
          path => "/home/drets/log.txt.processed"
          sincedb_path => "/home/drets/.log.txt.sincedb"
          codec => "json"
          start_position => "beginning"
          tags => [ "awesomewm" ]
          type => "awesomewm"
        }
      '';
      outputConfig = ''
        elasticsearch {
          index => "quantified-self"
          document_type => "awesomewm"
        }
      '';
    };

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
  programs.fish.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  virtualisation.virtualbox.host.enable = true;

  environment = {
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
      conky
      dolphin
      deadbeef
      diffutils
      dropbox
      emacs
      epdfview
      file
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
      hexchat
      htop
      libreoffice
      man-pages
      mplayer
      networkmanagerapplet
      openvpn
      patchelf
      p7zip
      python
      python3
      qbittorrent
      rsync
      plasma-workspace
      scrot
      shared_mime_info
      shutter
      slack
      stack
      skype
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
      xfce.xfce4-terminal
      xkbset
      xscreensaver
      zip
      pass
    ] ++ (with nixpkgs-16_09; [
      oxygen-gtk2
      oxygen-gtk3
    ]);
  };
}
