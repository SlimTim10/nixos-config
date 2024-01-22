{
  config,
  pkgs,
  flakePkgs,
  ...
}:

let
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
  
  # Take a screenshot (interactive selection) and copy the selection to the clipboard
  screenshot = pkgs.writeShellScriptBin "screenshot" ''
    maim -s | xclip -selection clipboard -t image/png
  '';

in {
  home.username = "tim";
  home.homeDirectory = "/home/tim";

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "video/mp4" = "mpv.desktop";
      "video/webm" = "mpv.desktop";
      "video/x-matroska" = "mpv.desktop";
      "video/x-msvideo" = "mpv.desktop";
      "application/pdf" = "firefox.desktop";
      "x-scheme-handler/mailto" = "thunderbird.desktop";
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" = "libreoffice.desktop";
      "image/gif" = "feh.desktop";
      "image/jpeg" = "feh.desktop";
      "image/png" = "feh.desktop";
      "image/svg+xml" = "feh.desktop";
      "image/tiff" = "feh.desktop";
      "image/webp" = "feh.desktop";
      "image/bmp" = "feh.desktop";
    };
  };

  programs.bash = {
    enable = true;
    enableCompletion = true;
    shellAliases = {
      pbcopy = "xclip -selection clipboard";
      cp = "rsync -avhP";
    };
  };

  programs.git = {
    enable = true;
    userName = "tim";
    userEmail = "slimtim10@gmail.com";
  };

  # For Haskell Reflex FRP
  # WARNING!!! This was causing nix to not use any binary caches, building every ephemeral shell package from source.
  # nix.settings.substituters = [ "https://nixcache.reflex-frp.org" ];
  # nix.settings.trusted-public-keys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  imports = [
    ./programs
  ];

  home.packages = with pkgs; [
    dmenu # app launcher

    # system tools
    htop # process monitor
    process-monitor # better visual process monitor
    wifi # my alias for nmtui
    hardinfo # hardware info GUI
    pavucontrol # audio controls GUI
    qdirstat # visual disk space analyzer

    # desktop visuals
    dzen2 # for displaying volume

    # password management
    keepassxc

    # email
    thunderbird

    # web browsing
    firefox # main web browser
    weather-forecast # shortcut for weather page
    google-chrome # for dev work
    freetube # open source YouTube client

    # music and video players
    vlc
    spotify

    # programming/development
    cabal2nix
    cabal-install
    nix-prefetch-git
    ghc
    agda
    nodejs

    # accounting
    hledger
    hledger-web # web UI

    # social/work
    unstable.zoom-us # video meetings
    unstable.slack # HiDPI resolution fix: --force-device-scale-factor=1.5

    # for Android
    jmtpfs # file transfers

    # my packages
    flakePkgs.easy-invoice-maker

    # misc
    deluge # torrent client
    xfce.thunar # graphical file manager
    megasync # MEGA cloud storage
    xclip # clipboard help
    maim # screenshot utility
    screenshot # uses maim (bind to PrtSc key)
    libreoffice # for various writing formats
    ripgrep # grep alternative
    feh # image viewer
  ];

  # This value determines the home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update home Manager without changing this value. See
  # the home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  # Let home Manager install and manage itself.
  programs.home-manager.enable = true;
}
