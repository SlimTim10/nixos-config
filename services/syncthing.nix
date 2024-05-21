{ config, lib, pkgs, ... }:

{
  # Syncthing
  services.syncthing = {
    enable = true;
    user = "tim";
    dataDir = "/home/tim/Sync";
    configDir = "/home/tim/.config/syncthing";
    overrideDevices = true; # overrides any devices added or deleted through the WebUI
    overrideFolders = true; # overrides any folders added or deleted through the WebUI
    settings = {
      folders = {
        "Sync" = { # Name of folder in Syncthing, also the folder ID
          path = "/home/tim/Sync"; # Which folder to add to Syncthing
        };
      };
      "minHomeDiskFree" = {
        unit = "GB";
        value = 5;
      };
    };
  };
}
