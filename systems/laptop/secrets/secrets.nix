let
  laptop = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINoGKKZFQPk7DxtQhF13YmBK2cAQs+peLbDqr3bzc1Bt slimtim10@gmail.com";
in {
  "syncthingApiKey.age".publicKeys = [ laptop ];
}
