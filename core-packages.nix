{
  config,
  agenix,
  pkgs,
  ...
}:

let  
  hibernate = pkgs.writeShellScriptBin "hibernate" ''
    systemctl hibernate
  '';
  
  lock-screen = pkgs.writeShellScriptBin "lock-screen" ''
    slock
  '';

  # Swap ctrl and alt, and map capslock to alt
  myKeyboardLayout = pkgs.writeText "xkb-layout" ''
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

  set-keyboard-layout = pkgs.writeShellScriptBin "set-keyboard-layout" ''
    udevadm settle # Wait for all device events to be processed
    ${pkgs.xorg.xmodmap}/bin/xmodmap ${myKeyboardLayout}
  '';
    
in
{
  systemd.user.services."hotplug-keyboard" = {
    enable = true;
    description = "Load my keyboard modifications";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = false;
      ExecStartPre = "${pkgs.coreutils}/bin/sleep 2"; # Delay start by 2 seconds
      ExecStart = "${set-keyboard-layout}/bin/set-keyboard-layout";
      Restart = "on-failure";
      TimeoutStartSec = 2;
      RestartSec = 2; # Wait 2 seconds before attempting to restart
    };
  };
  
  services.udev.extraRules = ''
    ACTION=="add|change", SUBSYSTEMS=="usb", TAG+="systemd", ENV{SYSTEMD_USER_WANTS}+="hotplug-keyboard.service"
    ACTION=="add|change", SUBSYSTEMS=="bluetooth", TAG+="systemd", ENV{SYSTEMD_USER_WANTS}+="hotplug-keyboard.service"
  '';
  
  # List packages installed in system profile.
  environment.systemPackages = with pkgs; [
    # essentials
    git
    vim
    wget
    agenix.packages."${system}".default

    # misc tools
    hibernate
    lock-screen
    set-keyboard-layout
  ];
}
