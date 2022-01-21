{ config, pkgs, ... }:

let
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    dm-tool lock
    systemctl hibernate
  '';
in
{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    hibernate
    git
    emacs
    vim
    wget
    dmenu # app launcher
    xclip # clipboard help
    keepass
    thunderbird # email
    firefox
    google-chrome
    zoom-us
    hardinfo # hardware info GUI
    pavucontrol # audio controls GUI
  ];
}
