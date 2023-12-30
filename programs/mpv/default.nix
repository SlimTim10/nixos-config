{
  pkgs,
  ...
}:

{
  home.file.".config/mpv" = {
    source = ./config;
  };

  home.packages = with pkgs; [
    mpv
  ];
}
