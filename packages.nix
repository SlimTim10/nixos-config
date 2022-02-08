{ config, pkgs, ... }:

let
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    dm-tool lock
    systemctl hibernate
  '';
  lock-screen = pkgs.writeShellScriptBin "lock-screen" ''
    dm-tool lock
  '';
in
{
  imports =
    [
      ./packages/dropbox.nix
      ./packages/spotify.nix
      ./packages/steam.nix
    ];

  # For Haskell Reflex FRP
  nix.binaryCaches = [ "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
  
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    hibernate
    lock-screen
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
    nodejs
    qbittorrent
    # haskellPackages.xmobar
    # xmobar
  ];
}
