{ config, lib, pkgs, ... }:

{
  networking.hostName = "tim-desktop";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp38s0.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;

  users.users.tim.extraGroups = [ "docker" ];

  imports = [
    ../../services/xmonad.nix
    ../../services/syncthing.nix
    ../../services/plex.nix
  ];

  # Syncthing
  services.syncthing = {
    settings = {
      devices = {
        "phone" = { id = "E7Q2U2F-6QQW3BO-ZEEURSH-A24UNTB-7FRH5HW-YB6IPPT-HR52YXY-ORQUGAX"; };
        "laptop" = { id = "DJO2T7I-CUAIJGW-QH2HXMF-6JW6F5O-XFNKKYD-FQDPBZW-GCAXHSW-IDITKQL"; };
        "mercury" = { id = "FDYSTDU-SJWNZS6-OMCBWY2-PBCTLTW-FYWXBQF-6JCQHES-JJUOGOB-5GVGMAI"; };
      };
      folders = {
        "Sync" = {
          devices = [ "phone" "laptop" "mercury" ]; # Which devices to share the folder with
        };
      };
    };
  };

  # Enable PostgreSQL
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_14;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 14 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
  };

  # Docker
  virtualisation.docker.enable = true;

  # Thumbnail support for images in thunar
  services.tumbler.enable = true; 

  # Open port for Spotify
  networking.firewall.allowedTCPPorts = [ 57621 ];

  # Steam
  programs.steam = {
    enable = true;
    # remotePlay.openFirewall = true; # Open ports in the firewall for Steam Remote Play
    # dedicatedServer.openFirewall = true; # Open ports in the firewall for Source Dedicated Server
  };

  # Bluetooth
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/hardware/bluetooth.nix
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    # See settings options: https://github.com/bluez/bluez/blob/master/src/main.conf
    settings = {
      General = {
        Name = "Hello";
        ControllerMode = "dual";
        FastConnectable = "true";
        # Experimental = "true";
      };
      Policy = {
        AutoEnable = "true";
      };
    };
  };
  services.blueman.enable = true;

  # Secrets
  age.identityPaths = [
    "/home/tim/.ssh/id_ed25519"
  ];
  age.secrets."syncthingApiKey" = {
    file = ./secrets/syncthingApiKey.age;
    mode = "700";
    owner = "tim";
    group = "users";
  };
}
