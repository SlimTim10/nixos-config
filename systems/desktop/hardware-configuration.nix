# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/2645b5fc-4d53-4bca-9421-4a8cfb534235";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/4E9F-1723";
      fsType = "vfat";
    };

  fileSystems."/media" =
    { device = "systemd-1";
      fsType = "autofs";
    };

  fileSystems."/mnt/external-hdd" =
    { device = "/dev/disk/by-uuid/ffe1e3ea-4c71-48dc-b063-80e7a127e0d7";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/49c75c29-cf75-40ca-96c2-6e3d8a121ff0"; }
    ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.br-11be9c8687ec.useDHCP = lib.mkDefault true;
  # networking.interfaces.br-c8e488edb830.useDHCP = lib.mkDefault true;
  # networking.interfaces.br-ffcab921686b.useDHCP = lib.mkDefault true;
  # networking.interfaces.docker0.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp38s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlo1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
