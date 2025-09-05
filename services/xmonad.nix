{ config, lib, pkgs, ... }:

{
  # XMonad
  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ../programs/xmonad/xmonad.hs;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad
        haskellPackages.xmonad-extras
        haskellPackages.xmonad-contrib
        haskellPackages.aeson
        haskellPackages.string-conversions
        haskellPackages.errors
      ];
    };
    displayManager.defaultSession = "none+xmonad";
    autorun = true;
  };

  # Use my keymap
  services.xserver.displayManager.sessionCommands = "set-keyboard-layout";
}
