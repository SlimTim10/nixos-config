{ config, lib, pkgs, ... }:

let
  set-screen-layout = pkgs.writeShellScript "set-screen-layout" ''
    export DISPLAY=:0

    function connect(){
        ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1-1 --off --output HDMI-0 --auto --primary
    }

    function disconnect(){
        ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1-1 --auto --primary --output HDMI-0 --off
    }

    ${pkgs.xorg.xrandr}/bin/xrandr | grep "HDMI-0 connected" &> /dev/null && connect || disconnect
  '';

  mercury = import "${
    builtins.fetchGit {
      url = "git@github.com:MercuryTechnologies/nixos-configuration.git";
      ref = "main";
      rev = "8d1b65f5640678e1503c69daf6e511ac2d37688c";
    }
  }/nixos-modules";
in {
  networking.hostName = "tim-mercury";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  imports = [
    ../../services/xmonad.nix
    ../../services/syncthing.nix
    ./certs
    mercury
  ];

  # Syncthing
  services.syncthing = {
    settings = {
      devices = {
        "phone" = { id = "E7Q2U2F-6QQW3BO-ZEEURSH-A24UNTB-7FRH5HW-YB6IPPT-HR52YXY-ORQUGAX"; };
        "desktop" = { id = "WXSYB7W-7NFJOM3-TIV7ZLD-OAZMBVF-ILSYQR7-DYYBZHD-MCUHF4H-O6IYZAU"; };
        "laptop" = { id = "DJO2T7I-CUAIJGW-QH2HXMF-6JW6F5O-XFNKKYD-FQDPBZW-GCAXHSW-IDITKQL"; };
      };
      folders = {
        "Sync" = {
          devices = [ "phone" "desktop" "laptop" ]; # Which devices to share the folder with
        };
      };
    };
  };

  # Thumbnail support for images in thunar
  services.tumbler.enable = true; 

  # Open port for Spotify
  networking.firewall.allowedTCPPorts = [ 57621 ];

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
    settings = {
      General = {
        Name = "Hello";
        ControllerMode = "dual";
        FastConnectable = "true";
        Experimental = "true";
      };
      Policy = {
        AutoEnable = "true";
      };
    };
  };
  services.blueman.enable = true;

  # External monitor
  systemd.user.services."hotplug-hdmi" = {
    enable = true;
    description = "Load my screen layout";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = false;
      ExecStart = "${set-screen-layout}";
    };
  };
  services.udev.extraRules = ''
    ACTION=="change", KERNEL=="card0", SUBSYSTEM=="drm", ENV{DISPLAY}=":0", TAG+="systemd", ENV{SYSTEMD_USER_WANTS}+="hotplug-hdmi.service"
  '';

  # Secrets
  age = {
    identityPaths = [ "/home/tim/.ssh/id_ed25519" ];
    secrets = {
      "syncthingApiKey" = {
        file = ./secrets/syncthingApiKey.age;
        mode = "700";
        owner = "tim";
        group = "users";
      };
    };
  };

  mercury = {
    # Enable the CA cert used for internal resources
    internalCertificateAuthority.enable = true;

    # Enable services required for MWB development (Postgres)
    mwbDevelopment.enable = true;

    # Enable the internal Nix cache
    nixCache.enable = true;
  };

  # Tailscale
  services.tailscale.enable = true;

  # Kolide
  services.kolide-launcher.enable = true;
  environment.etc."kolide-k2/secret" = {
    mode = "0600";
    text = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJvcmdhbml6YXRpb24iOiJuYWtvbmUiLCJraWQiOiJiODoxZDowNjo5NzpjYjo3OTpjMDo3MTpjNDoxNTpjZDo5Yzo4Mjo0MDo4NjpjYSIsImNyZWF0ZWRBdCI6IjE3MDUxMTgwMzYiLCJjcmVhdGVkQnkiOiJrd29ya2VyIn0.vCMoj_pnDjEG3Ji9y8elRzN10QfFOwGxZrJAQcJWP41SmDN1PsLQusKucX7lwUTlfgm6-9mKLnaJ9uhA-2j0G2_J2TCP9KxyvZ2M2jH4x_5muf1kV99RgwJhhjlFbZU_9ri8ZZc-fOlaaFZi6hKg5GwaaLSNTex2HKzfcx3PVdDjaXoAKc-THHgtQ9-j_4P_co7JkxxCgnsqpMw13qm2nNZ5PAE2wOuU1_MdVeNam4MnLt1BBgxbeclCHfKjrcg-H9UDcQtwiYxllsfDSpmgfNDr2b69Y064UqKAjqWyvE33c-7hBx_R2HC9glXulmdijgPgGABT1Ad6zhA6QS8xTg";
  };
}
