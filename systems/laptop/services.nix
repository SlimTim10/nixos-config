{ config, lib, pkgs, ... }:

let
  # Swap ctrl and alt, and map capslock to alt
  myCustomLayout = pkgs.writeText "xkb-layout" ''
    ! Swap ctrl and alt, and map capslock to alt
    clear lock
    clear control
    clear mod1
    keycode 66 = Alt_L
    keycode 37 = Alt_L Meta_L
    keycode 105 = Alt_R Meta_R
    keycode 64 = Control_L
    keycode 108 = Control_R
    add control = Control_L Control_R
    add mod1 = Alt_L Meta_L
  '';
in {
  networking.hostName = "tim-laptop";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp38s0.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;

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
        "mercury" = { id = "SYOW4SS-XKWX2FY-5XT5LJK-WECQX2R-XCBF45P-5U65B5M-JT3SZD5-YDOX2QI"; };
      };
      folders = {
        "Sync" = { # Name of folder in Syncthing, also the folder ID
          path = "/home/tim/Sync"; # Which folder to add to Syncthing
          devices = [ "phone" "desktop" "mercury" ]; # Which devices to share the folder with
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
