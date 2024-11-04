{ config, ... }:

{
  # services.xserver.videoDrivers = [ "nvidia" ];

  # Enable touchpad support
  services.libinput.enable = true;
}
