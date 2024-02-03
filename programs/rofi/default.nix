{
  pkgs,
  ...
}:

let
  # Launch custom rofi launcher
  rofi-custom = pkgs.writeShellScriptBin "rofi-custom" ''
    ~/.config/rofi/launchers/type-1/launcher.sh
  '';
in {

  home.file.".config/rofi" = {
    source = ./config;
  };

  home.packages = with pkgs; [
    rofi
    rofi-custom
  ];
}
