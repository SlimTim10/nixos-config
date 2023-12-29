{ config, ... }:

{
  # NVIDIA drivers are unfree.
  nixpkgs.config.allowUnfree = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.opengl.enable = true;

  # Fix graphical corruption on suspend/resume
  hardware.nvidia.powerManagement.enable = true;

  # Fix screen tearing
  hardware.nvidia.forceFullCompositionPipeline = true;
}
