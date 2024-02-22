{ config, lib, pkgs, ... }:

{
  networking.hostName = "tim-desktop";

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
        "laptop" = { id = "DJO2T7I-CUAIJGW-QH2HXMF-6JW6F5O-XFNKKYD-FQDPBZW-GCAXHSW-IDITKQL"; };
        "mercury" = { id = "SYOW4SS-XKWX2FY-5XT5LJK-WECQX2R-XCBF45P-5U65B5M-JT3SZD5-YDOX2QI"; };
      };
      folders = {
        "Sync" = { # Name of folder in Syncthing, also the folder ID
          path = "/home/tim/Sync"; # Which folder to add to Syncthing
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
