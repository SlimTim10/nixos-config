let
  desktop = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMaUtCUyfQHn+qJvmr8nf0v83WwpOgBoNyqma71DsWR4 slimtim10@gmail.com";
in {
  "syncthingApiKey.age".publicKeys = [ desktop ];
}
