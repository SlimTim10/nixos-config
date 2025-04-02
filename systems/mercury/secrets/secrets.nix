let
  mercury = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICWzyr6jDbd7VMyTLOVeAazr9OAbrDKTVEBbXnlYqUfq slimtim10@gmail.com";
in {
  "syncthingApiKey.age".publicKeys = [ mercury ];
}
