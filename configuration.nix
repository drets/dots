{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  system.stateVersion = "16.09";

  nixpkgs.config.allowUnfree = true;
  nix.binaryCaches = [ "https://cache.nixos.org" ];

  hardware = {
    opengl.driSupport32Bit = true;
    enableAllFirmware = true;
    pulseaudio.enable = true;
    pulseaudio.support32Bit = true;
    bluetooth.enable = false;
  };

  powerManagement.enable = true;

  boot.loader.gummiboot.enable = true;
  boot.loader.gummiboot.timeout = 0;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [ "hid_apple.iso_layout=0" "hid_apple.fnmode=2" ];

  nix.useChroot = true;

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
    accelFactor = "0.001";
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

  services.logind.extraConfig = "HandlePowerKey=ignore";

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

  environment.pathsToLink = [ "/share" ];
  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.ru
    cabal-install
    cabal2nix
    emacs
    file
    firefox
    ghc
    gimp
    git
    gnumake
    gnupg
    google-chrome
    haskellPackages.ghc-mod
    htop
    kde4.gwenview
    kde4.kde_baseapps
    keepassx
    man-pages
    nix-repl
    nodePackages.jshint
    nodejs
    openvpn
    p7zip
    phantomjs
    python
    rxvt_unicode
    scrot
    shared_mime_info
    skype
    stack
    unzip
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
