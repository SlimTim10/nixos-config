{
  config,
  pkgs,
  flakePkgs,
  ...
}:

{
  programs.git = {
    userName = "Tim Johns";
    userEmail = "timj@mercury.com";
  };
}
