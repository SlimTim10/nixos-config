let
  mercury = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPr6JeuJ0hWTNYqhCAcIjowl2+M5S67vez9MFTVOT56Z slimtim10@gmail.com";
in {
  "syncthingApiKey.age".publicKeys = [ mercury ];
}
