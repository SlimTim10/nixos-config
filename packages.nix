{ config, pkgs, ... }:

let
  # sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
  # sudo nix-channel --update
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    dm-tool lock
    systemctl hibernate
  '';
  
  lock-screen = pkgs.writeShellScriptBin "lock-screen" ''
    dm-tool lock
  '';
  
  # For launching process monitor from dmenu
  process-monitor = pkgs.writeShellScriptBin "process-monitor" ''
    xterm -e htop
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
  
  # Take a screenshot (interactive selection) and copy the selection to the clipboard
  screenshot = pkgs.writeShellScriptBin "screenshot" ''
    maim -s | xclip -selection clipboard -t image/png
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
    imagemagick # for image-dired in emacs
    
    vim
    wget
    silver-searcher # ag (grep alternative)
    dmenu # app launcher
    
    htop # process monitor
    process-monitor # better visual process monitor
    
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
    slack # HiDPI resolution fix: --force-device-scale-factor=1.5
    mpv # video player
    nodejs
    qbittorrent # torrent client
    deluge # torrent client
    xmobar # status bar for xmonad
    xfce.thunar # graphical file manager
    libreoffice
    dzen2 # for displaying volume
    hledger # accounting
    
    maim # screenshot utility
    screenshot # uses maim (bind to PrtSc key)

    # haskell development
    cabal2nix
    cabal-install
    nix-prefetch-git

    # agda
    ghc
    zlib
  ];
  services.tumbler.enable = true; # thumbnail support for images in thunar
}
