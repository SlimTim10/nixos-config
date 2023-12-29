{
  description = "NixOS configuration of Tim Johns";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # easy-invoice-maker.url = "github:SlimTim10/easy-invoice-maker";

    #secrets management
    agenix = {
      # lock with git commit at 0.15.0
      url = "github:ryantm/agenix/564595d0ad4be7277e07fa63b5a991b3c645655d";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ nixpkgs, home-manager, agenix, ... }:
    let
      # Common
      system = "x86_64-linux";
      specialArgs = inputs;
      modules = [
        ./configuration.nix
        agenix.nixosModules.default
      ];
    in {
      nixosConfigurations = {
        # Run the following command in the flake's directory to
        # deploy this configuration on any NixOS system:
        #   sudo nixos-rebuild switch --flake .#desktop
        desktop = nixpkgs.lib.nixosSystem {
          inherit system;
          inherit specialArgs;
          modules = modules ++ [
            ./systems/desktop/hardware.nix
            ./systems/desktop/hardware-configuration.nix
            ./systems/desktop/services.nix
            # agenix.nixosModules.age
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              # home-manager.users.tim = import ./home.nix;
              home-manager.users.tim.imports = [ ./home.nix ];
            }
          ];
        };
      };
    };
}
