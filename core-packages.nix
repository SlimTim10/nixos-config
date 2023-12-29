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
    
in
{
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
  ];
}
