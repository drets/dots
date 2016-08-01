{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  system.stateVersion = "16.09";

  nixpkgs.config.allowUnfree = true;
  nix.binaryCaches = [ "https://cache.nixos.org" ];

  nix.nixPath = [
    "/nix/var/nix/profiles/per-user/root/channels/nixos"
    "nixos-config=/home/drets/src/dots/nixos/configuration.nix"
    "/nix/var/nix/profiles/per-user/root/channels"
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
    timeout = 0;
    efi.canTouchEfiVariables = true;
  };

  boot.kernelParams = [ "hid_apple.iso_layout=0" "hid_apple.fnmode=2" ];
  boot.tmpOnTmpfs = true;
  boot.blacklistedKernelModules = ["bdc_pci"];

  nix.useSandbox = true;

  boot.initrd.luks.devices = [{
    name = "rootfs";
    device = "/dev/sda5";
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

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Amsterdam";

  services.tlp.enable = true;
  services.xserver.enable = true;
  services.xserver.layout = "us,ru";
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
    luaModules = [ pkgs.luaPackages.luafilesystem ];
  };
  services.xserver.xkbOptions = "grp:lctrl_toggle,ctrl:nocaps";
  services.redshift = {
    enable = true;
    latitude = "52.39";
    longitude = "16.96";
  };

  users.extraUsers.drets = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "users" "wheel" "dialout" "networkmanager" ];
    initialPassword = "foobar";
  };

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = false;

    fonts = with pkgs; [
      powerline-fonts
      inconsolata
      corefonts
      terminus_font
      dejavu_fonts
      source-code-pro
      ubuntu_font_family
      unifont
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

  services.openvpn.servers = {
    wikia.config = ''
      client
      dev tun
      proto tcp
      remote 91.102.115.105 1194
      resolv-retry infinite
      nobind
      persist-key
      persist-tun
      ca /home/drets/.vpn/ca.crt
      cert /home/drets/.vpn/dmytro.rets@wikia-inc.com.crt
      key /home/drets/.vpn/dmytro.rets@wikia-inc.com.key
      comp-lzo
      verb 3
    '';
  };

  programs.kbdlight.enable = true;
  programs.zsh.enable = true;

  users.defaultUserShell = "/run/current-system/sw/bin/fish";

  services.mysql = {
    enable = true;
    package = pkgs.mysql;
    extraOptions = "max_allowed_packet=100M";
  };

  services.httpd = {
    enable = true;
    enablePHP = true;
    logPerVirtualHost = true;
    adminAddr="drets@wikia-inc.com";
    documentRoot = "/var/upstream/";
    virtualHosts = [{
      hostName = "fandom";
      serverAliases = [ "fandom.dev" ];
    }];
    extraConfig = ''
      <Directory /var/upstream/>
        DirectoryIndex index.php
        Allow from *
        Options FollowSymLinks
        AllowOverride All
      </Directory>
    '';
  };

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  systemd.services.disableDeviceSuspending = {
    wantedBy = [ "default.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.bash}/bin/bash -c '${/home/drets/bin/suspend-disable-device} LID0 XHC1'";
    };
  };

  systemd.services.disableDeviceSuspending.enable = true;

  environment.pathsToLink = [ "/share" ];
  environment.systemPackages = with pkgs; [
    ag
    aspell
    aspellDicts.en
    aspellDicts.ru
    cabal-install
    cabal2nix
    chromedriver
    emacs
    file
    firefox
    fish
    gimp
    git
    gnumake
    gnupg
    global
    goldendict
    google-chrome
    haskell.packages.ghc7103.ghc
    haskell.packages.ghc7103.ghc-mod
    haskell.packages.ghc7103.hasktags
    haskell.packages.ghc7103.structured-haskell-mode
    haskell.packages.ghc7103.hlint
    haskell.packages.ghc7103.hspec-discover
    haskell.packages.ghc7103.stylish-haskell
    hexchat
    htop
    idea.idea-community
    jenkins-job-builder
    kde4.gwenview
    kde4.kde_baseapps
    keepassx
    man-pages
    mplayer
    nix-repl
    nodePackages.jshint
    nodejs
    nox
    openjdk
    openvpn
    p7zip
    phantomjs
    python
    qbittorrent
    rxvt_unicode
    scrot
    shared_mime_info
    skype
    stack
    thunderbird
    tty-clock
    unzip
    unclutter
    vlc
    wget
    which
    whois
    xclip
    xorg.xbacklight
    xscreensaver
    zip
  ];
}
