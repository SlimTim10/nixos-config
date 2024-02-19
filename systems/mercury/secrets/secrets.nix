let
  mercury = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHyKGHcNwmBcQJ2wGMJV820fG+0Xw1rj2b1Ew+jAEP3K slimtim10@gmail.com";
in {
  "syncthingApiKey.age".publicKeys = [ mercury ];
}
