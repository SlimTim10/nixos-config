{ config, lib, pkgs, ... }:

{
  nix.settings.substituters = [
    "https://cache.hub.internal.mercury.com"
    "https://cache.mercury.com"
  ];

  nix.settings.trusted-public-keys = [
    "cache.hub.internal.mercury.com:yhfFlgvqtv0cAxzflJ0aZW3mbulx4+5EOZm6k3oML+I="
    "cache.mercury.com:yhfFlgvqtv0cAxzflJ0aZW3mbulx4+5EOZm6k3oML+I="
  ];
}
