{ config, lib, pkgs, ... }:

{
  # XMonad
  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ../programs/xmonad/xmonad.hs;
    };
    displayManager.defaultSession = "none+xmonad";
    autorun = true;
  };

  # Use my keymap
  services.xserver.displayManager.sessionCommands = "set-keyboard-layout";
}
