{ config, ... }:

{
  # services.xserver.videoDrivers = [ "nvidia" ];

  # Enable touchpad support
  services.xserver.libinput.enable = true;
}
