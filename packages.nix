{ config, pkgs, ... }:

let
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    echo "Hibernating..."
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
    dmenu
    xclip
    keepass
    thunderbird
    firefox # 29.0.1
    google-chrome
    zoom-us
    hardinfo
  ];
}
