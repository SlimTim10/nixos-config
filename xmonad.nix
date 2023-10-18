{ config, ... }:

# If xmonad --recompile complains about missing XMonad, use a shell that has it:
# nix-shell -p "ghc.withPackages (pkgs: with pkgs; [ xmonad xmonad-extras xmonad-contrib ])"
{
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  services.xserver.autorun = true;
  services.xserver.displayManager.defaultSession = "none+xmonad";

  services.xserver.windowManager.xmonad.config = builtins.readFile /home/tim/.nix-config/xmonad.hs;
}
