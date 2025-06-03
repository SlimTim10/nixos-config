let
  mercury = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIt3wtB4p/Zw9MkrEEFxpHJ+TE1n2cy+fdu+WIyCfVXd slimtim10@gmail.com";
in {
  "syncthingApiKey.age".publicKeys = [ mercury ];
}
