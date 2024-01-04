{
  pkgs,
  ...
}:

{
  home.file.".emacs.d" = {
    source = ./.emacs.d;
    recursive = true;
  };
  home.file.".emacs" = {
    source = ./.emacs;
  };

  home.packages = with pkgs; [
    unstable.emacs
    imagemagick # for image-dired in emacs
    silver-searcher # ag (grep alternative)
  ];
}
