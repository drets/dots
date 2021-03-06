# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, ... }:

{
  imports =
    [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" "vboxdrv" "vboxnetadp" "vboxnetflt"];
  boot.kernelModules = [ "kvm-intel" "wl" ];
  boot.extraModulePackages = [
    config.boot.kernelPackages.broadcom_sta
  ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/c232d4a9-bedf-4594-914e-f07a07ca8642";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/908A-B609";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/27aa9ab2-4b00-4824-b3ac-3e9932cc2a65"; }
    ];

  nix.maxJobs = lib.mkDefault 4;
}
