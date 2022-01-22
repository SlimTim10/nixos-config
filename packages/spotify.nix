# Open port for Spotify
{ config, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [ 57621 ];

  environment.systemPackages = with pkgs; [
    spotify
  ];
}
