{ config, ... }:

{
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.autorun = true;
  services.xserver.displayManager.defaultSession = "none+xmonad";
}
