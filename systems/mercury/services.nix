{ config, lib, pkgs, ... }:

let
  set-screen-layout = pkgs.writeShellScript "set-screen-layout" ''
    export DISPLAY=:0

    function connect(){
        ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1 --off --output HDMI-1 --auto --primary
    }

    function disconnect(){
        ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1 --auto --primary --output HDMI-1 --off
    }

    ${pkgs.xorg.xrandr}/bin/xrandr | grep "HDMI-1 connected" &> /dev/null && connect || disconnect
  '';
in {
  networking.hostName = "tim-mercury";

  imports = [
    ../../services/xmonad.nix
  ];

  # Syncthing
  services.syncthing = {
    enable = true;
    user = "tim";
    dataDir = "/home/tim/Sync";
    configDir = "/home/tim/.config/syncthing";
    overrideDevices = true; # overrides any devices added or deleted through the WebUI
    overrideFolders = true; # overrides any folders added or deleted through the WebUI
    settings = {
      devices = {
        "phone" = { id = "E7Q2U2F-6QQW3BO-ZEEURSH-A24UNTB-7FRH5HW-YB6IPPT-HR52YXY-ORQUGAX"; };
        "desktop" = { id = "WXSYB7W-7NFJOM3-TIV7ZLD-OAZMBVF-ILSYQR7-DYYBZHD-MCUHF4H-O6IYZAU"; };
        "laptop" = { id = "DJO2T7I-CUAIJGW-QH2HXMF-6JW6F5O-XFNKKYD-FQDPBZW-GCAXHSW-IDITKQL"; };
      };
      folders = {
        "Sync" = { # Name of folder in Syncthing, also the folder ID
          path = "/home/tim/Sync"; # Which folder to add to Syncthing
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
  hardware.bluetooth.enable = true; # enables support for Bluetooth
  hardware.bluetooth.powerOnBoot = true; # powers up the default Bluetooth controller on boot
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
