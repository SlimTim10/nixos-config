{ config, pkgs, ... }:

let
  # sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
  # sudo nix-channel --update
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    systemctl hibernate
  '';
  
  lock-screen = pkgs.writeShellScriptBin "lock-screen" ''
    slock
  '';
  
  # For launching process monitor from dmenu
  process-monitor = pkgs.writeShellScriptBin "process-monitor" ''
    xterm -e htop
  '';
  
  # For launching nmtui (network manager) from dmenu (aliased as "wifi" for easier remembrance)
  wifi = pkgs.writeShellScriptBin "wifi" ''
    xterm -e nmtui
  '';
  
  # Launch firefox with Toronto weather forecast
  weather-forecast = pkgs.writeShellScriptBin "weather-forecast" ''
    firefox --new-window https://weather.gc.ca/city/pages/on-143_metric_e.html
  '';
  
  # Restart dropbox
  restart-dropbox = pkgs.writeShellScriptBin "restart-dropbox" ''
    dropbox stop
    sleep 5
    dropbox start
  '';

  # Dropbox status for xmobar
  xmobar-dropbox-status = pkgs.writeShellScriptBin "xmobar-dropbox-status" ''
    status="$(dropbox status)"

    case "$status" in
      "Up to date")
        echo "Up to date" ;;
      Syncing*)
        echo "Syncing..." ;;
      *)
        echo "Dropbox: $status" ;;
    esac
  '';
  
  # Take a screenshot (interactive selection) and copy the selection to the clipboard
  screenshot = pkgs.writeShellScriptBin "screenshot" ''
    maim -s | xclip -selection clipboard -t image/png
  '';

  haskell-invoice = (import (builtins.fetchGit {
    name = "haskell-invoice";
    url = "https://github.com/SlimTim10/haskell-invoice/";
  }));

in
{
  imports =
    [
      ./packages/dropbox.nix
      ./packages/spotify.nix
      ./packages/steam.nix
    ];

  # For Haskell Reflex FRP
  nix.settings.substituters = [ "https://nixcache.reflex-frp.org" ];
  nix.settings.trusted-public-keys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
  
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    hibernate
    lock-screen
    
    git
    
    emacs
    imagemagick # for image-dired in emacs
    
    vim
    wget
    silver-searcher # ag (grep alternative)
    dmenu # app launcher
    
    htop # process monitor
    process-monitor # better visual process monitor
    wifi # my alias for nmtui
    
    xmobar # status bar for xmonad
    xmobar-dropbox-status
    
    xclip # clipboard help
    keepassxc # password manager
    thunderbird # email
    firefox # main web browser
    weather-forecast # shortcut for weather page
    restart-dropbox # necessary because dropbox linux is buggy
    google-chrome # for dev work
    unstable.zoom-us # video meetings
    hardinfo # hardware info GUI
    pavucontrol # audio controls GUI
    unstable.slack # HiDPI resolution fix: --force-device-scale-factor=1.5
    mpv # video player
    vlc # video player
    nodejs
    qbittorrent # torrent client
    deluge # torrent client    
    xfce.thunar # graphical file manager
    libreoffice
    dzen2 # for displaying volume
    hledger # accounting
    qdirstat # visual disk space analyzer
    
    maim # screenshot utility
    screenshot # uses maim (bind to PrtSc key)

    # haskell development
    cabal2nix
    cabal-install
    nix-prefetch-git

    # agda
    ghc
    zlib

    # packages I created
    haskell-invoice
  ];
  services.tumbler.enable = true; # thumbnail support for images in thunar
}
