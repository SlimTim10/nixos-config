# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
#  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
#    export __NV_PRIME_RENDER_OFFLOAD=1
#    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
#    export __GLX_VENDOR_LIBRARY_NAME=nvidia
#    export __VK_LAYER_NV_optimus=NVIDIA_only
#    exec -a "$0" "$@"
#  '';
  myCustomLayout = pkgs.writeText "xkb-layout" ''
    ! Swap ctrl and alt, and map capslock to alt
    clear lock
    clear control
    clear mod1
    keycode 66 = Alt_L
    keycode 37 = Alt_L Meta_L
    keycode 105 = Alt_R Meta_R
    keycode 64 = Control_L
    keycode 108 = Control_R
    add control = Control_L Control_R
    add mod1 = Alt_L Meta_L
  '';
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    echo "Hibernating..."
    dm-tool lock
    systemctl hibernate
  '';
in
{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./xmonad.nix
      # ./xfce.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Run scripts at startup
  system.activationScripts = {
    linkXmonadConfig = ''
      ln -sf /home/tim/.nix-config/xmonad.hs /home/tim/.xmonad/xmonad.hs
    '';
    linkEmacsConfig = ''
      ln -sf /home/tim/emacs-home/.emacs.d/ /home/tim/.emacs.d
      ln -sf /home/tim/emacs-home/.emacs /home/tim/.emacs
    '';
  };

  environment.interactiveShellInit = ''
    alias pbcopy='xclip -selection clipboard'
  '';

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.loader.grub.configurationLimit = 4;

  # Networking
  networking.hostName = "nixos"; # Define your hostname.
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  #networking.wireless.networks = {
  #  "networkname" = {
  #    psk = "networkpass";
  #  };
  #};
  networking.networkmanager.enable = true;

  # Enables flakes
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp38s0.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Set resolution
  # TODO: make automatic (4K monitor resolution needs DPI scaling)
  # xrandr -s 3840x2160 --output HDMI-A-0 --scale-from 1440x900
  services.xserver.resolutions = [ { x = 1440; y = 900; } ];

  # Keymap
  services.xserver.displayManager.sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${myCustomLayout}";
      
  # Settings for Nvidia graphics card (should be in separate file)
  #services.xserver.videoDrivers = [ "nvidia" ];
  # Offload
  #hardware.nvidia = {
  #  # beta drivers
  #  package = config.boot.kernelPackages.nvidiaPackages.beta;
  #  # fixes a glitch
  #  nvidiaPersistenced = true;
  #  # required for amd gpu and nvidia gpu pairings
  #  modesetting.enable = true;
  #  prime = {
  #    offload.enable = true;
  #    #sync.enable = true;
  #    amdgpuBusId = "PCI:30:0:0";
  #    nvidiaBusId = "PCI:10:0:0";
  #  };
  #};
  #hardware.nvidia.modesetting.enable = true;
  #hardware.nvidia.prime = {
  #  sync.enable = true;
  #  nvidiaBusId = "PCI:10:0:0";
  #  amdgpuBusId = "PCI:30:0:0";
  #};

  # Configure keymap in X11
  services.xserver.layout = "us";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tim = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    home = "/home/tim";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    hibernate
    git
    emacs
    vim
    wget
    dmenu
    xclip
    keepass
    thunderbird
    firefox # 29.0.1
    google-chrome
    #nvidia-offload
    zoom-us
    hardinfo
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?
  #system.stateVersion = "unstable";

}

