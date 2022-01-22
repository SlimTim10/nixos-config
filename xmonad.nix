{ config, ... }:

{
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  services.xserver.autorun = true;
  services.xserver.displayManager.defaultSession = "none+xmonad";
}
