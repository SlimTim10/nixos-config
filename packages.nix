{ config, pkgs, ... }:

let
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    dm-tool lock
    systemctl hibernate
  '';
in
{
  imports =
    [
      ./packages/dropbox.nix
      ./packages/spotify.nix
    ];
  
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    hibernate
    git
    emacs
    vim
    wget
    dmenu # app launcher
    htop # process monitor
    xclip # clipboard help
    keepassxc
    thunderbird # email
    firefox
    google-chrome
    zoom-us
    hardinfo # hardware info GUI
    pavucontrol # audio controls GUI
    slack # HiDPI resolution fix: --force-device-scale-factor=1.5
    mpv
  ];
}
