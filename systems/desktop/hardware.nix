{ config, ... }:

{
  # NVIDIA drivers are unfree.
  nixpkgs.config.allowUnfree = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;

  hardware.nvidia = {
    # Fix graphical corruption on suspend/resume
    powerManagement.enable = true;

    # Fix screen tearing
    forceFullCompositionPipeline = true;
  };
}
