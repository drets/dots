{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  system.stateVersion = "16.09";

  nixpkgs.config.allowUnfree = true;
  nix.binaryCaches = [ "https://cache.nixos.org" ];

  nix.nixPath =
    let dotfiles = "/home/drets/src/dots";
    in [
      "nixos-config=${dotfiles}/nixos/configuration.nix"
      "${dotfiles}/channels"
    ];

  hardware = {
    bluetooth.enable = false;
    enableAllFirmware = true;
    facetimehd.enable = true;
    opengl.driSupport32Bit = true;
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
  };

  powerManagement.enable = true;

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    timeout = 0;
  };

  boot.kernelParams = [ "hid_apple.iso_layout=0" "hid_apple.fnmode=2" ];
  boot.blacklistedKernelModules = ["bdc_pci"];

  nix.useSandbox = true;

  boot.initrd.luks.devices = [{
    name = "rootfs";
    device = "/dev/sda3";
    preLVM = true;
  }];

  networking = {
    hostName = "MBP";

    useDHCP = false;
    wicd.enable = true;
    wireless.enable = false;
  };

  services.dnsmasq = {
    enable = true;

    # These are used in addition to resolv.conf
    servers = [ "8.8.8.8" "8.8.4.4" ];

    extraConfig = ''
      listen-address=127.0.0.1
      cache-size=1000

      no-negcache
    '';
  };

  # Put the text in /etc/resolv.conf.head
  #
  # That will prepend dnsmasq server to /etc/resolv.conf (dhcpcd-specific)
  environment.etc."resolv.conf.head".text = ''
    nameserver 127.0.0.1
  '';

  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" ];

  time.timeZone = "Europe/Amsterdam";

  services.locate = {
    enable = true;
    localuser = "drets";
    interval = "*-*-* 17:00:00";
    extraFlags = ["--localpaths='/home/drets'"];
  };

  services.xserver.enable = true;
  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    vertEdgeScroll = true;
    accelFactor = "0.1";
    maxSpeed = "1.5";
    buttonsMap = [ 1 3 2 ];
    palmDetect = true;
  };

  services.xserver.windowManager.awesome = {
    enable = true;
    luaModules = [ pkgs.luaPackages.luafilesystem pkgs.luaPackages.cjson ];
  };
  services.redshift = {
    enable = true;
    latitude = "52.39";
    longitude = "16.96";
  };

  users.extraGroups.plugdev = { };
  users.extraUsers.drets = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "plugdev" "dialout" "users" "wheel" "dialout" "networkmanager" ];
    initialPassword = "foobar";
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = false;

    fonts = with pkgs; [
      corefonts
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

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
    HandlePowerKey=ignore
  '';

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  programs.kbdlight.enable = true;
  programs.zsh.enable = true;

  users.defaultUserShell = pkgs.zsh;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  systemd.services.disableDeviceSuspending = {
    enable = true;
    wantedBy = [ "default.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c '${/home/drets/bin/suspend-disable-device} LID0 XHC1'";
    };
  };

  systemd.services.healthySleep = {
    script = "shutdown -P";
  };

  services.postgresql.enable = true;
  services.postgresql.authentication = "local all all ident";

  systemd.timers.healthySleep = {
    partOf = [ "healthySleep.service" ];
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "*-*-* 00:06:59";
  };

  environment.shellInit = ''
    export GTK_PATH=$GTK_PATH:${pkgs.oxygen_gtk}/lib/gtk-2.0
    export GTK2_RC_FILES=$GTK2_RC_FILES:${pkgs.oxygen_gtk}/share/themes/oxygen-gtk/gtk-2.0/gtkrc
  '';

  environment.pathsToLink = [ "/share" ];
  environment.systemPackages = with pkgs; [
    ag
    anki
    aspell
    aspellDicts.en
    aspellDicts.ru
    cabal-install
    chromedriver
    deadbeef
    diffutils
    dropbox
    emacs
    file
    firefox
    gcc
    gimp
    git
    global
    gnome3.adwaita-icon-theme
    gnome3.gconf
    gnumake
    gnupg
    goldendict
    google-chrome
    haskellPackages.ghc
    haskellPackages.hasktags
    haskellPackages.hlint
    hexchat
    htop
    kde4.gwenview
    kde4.kde_baseapps
    kde4.kwin_styles
    kde4.oxygen_icons
    keepassx
    libreoffice
    man-pages
    mplayer
    nix-repl
    nodePackages.jshint
    nodejs
    nox
    openjdk
    openvpn
    oxygen-gtk2
    oxygen-gtk3
    p7zip
    phantomjs
    python
    python3
    python3Packages.matplotlib
    qbittorrent
    rxvt_unicode
    rsync
    scrot
    shared_mime_info
    shutter
    skype
    slack
    stack
    tightvnc
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
    xscreensaver
    zathura
    zip
  ];
}
