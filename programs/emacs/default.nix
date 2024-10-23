{
  pkgs,
  ...
}:

let
  myEmacs = (pkgs.emacsPackagesFor pkgs.unstable.emacs).emacsWithPackages (epkgs: with epkgs; [
    treesit-grammars.with-all-grammars
  ]);
in {
  home.file.".emacs.d" = {
    source = ./.emacs.d;
    recursive = true;
  };
  home.file.".emacs" = {
    source = ./.emacs;
  };

  home.packages = with pkgs; [
    myEmacs
    imagemagick # for image-dired in emacs
    silver-searcher # ag (grep alternative)
  ];

  # Run Emacs as a daemon
  services.emacs = {
    enable = true;
    package = myEmacs;
  };
}
