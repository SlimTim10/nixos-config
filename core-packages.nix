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
      ExecStart = "${set-keyboard-layout}/bin/set-keyboard-layout";
    };
  };
  
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEMS=="usb", TAG+="systemd", ENV{SYSTEMD_USER_WANTS}+="hotplug-keyboard.service"
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
