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
  # XMonad
  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = builtins.readFile ../../programs/xmonad/xmonad.hs;
    };
    displayManager.defaultSession = "none+xmonad";
    autorun = true;
  };

  # Use my keymap
  services.xserver.displayManager.sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}";

  # Thumbnail support for images in thunar
  services.tumbler.enable = true; 

  # Open port for Spotify
  networking.firewall.allowedTCPPorts = [ 57621 ];
}
