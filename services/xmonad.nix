{ config, lib, pkgs, ... }:

{
  # XMonad
  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ../programs/xmonad/xmonad.hs;
    };
    autorun = true;
  };
  services.displayManager.defaultSession = "none+xmonad";

  # Use my keymap
  services.xserver.displayManager.sessionCommands = "set-keyboard-layout";
}
