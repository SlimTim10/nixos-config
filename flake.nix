{
  description = "NixOS configuration of Tim Johns";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      # 'follows' is the inheritance syntax within inputs.
      # Here, it ensures that home-manager's 'inputs.nixpkgs' aligns with 
      # the current flake's inputs.nixpkgs,
      # avoiding inconsistencies in the dependency's nixpkgs version.
      inputs.nixpkgs.follows = "nixpkgs";
    };

    #secrets management
    agenix = {
      # lock with git commit at 0.15.0
      url = "github:ryantm/agenix/564595d0ad4be7277e07fa63b5a991b3c645655d";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    easy-invoice-maker = {
      url = "github:SlimTim10/easy-invoice-maker";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs
    , nixpkgs-unstable
    , home-manager
    , agenix
    , easy-invoice-maker
    , ...
    }@inputs:
    let
      # Common
      system = "x86_64-linux";
      specialArgs = inputs;
      overlay-unstable = final: prev: {
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
      };
      modules = [
        ./configuration.nix
        agenix.nixosModules.default
        ({ config, pkgs, ... }: { nixpkgs.overlays = [ overlay-unstable ]; })
      ];

      myFlakes = [
        "easy-invoice-maker"
      ];
      
      flakePkgs = builtins.mapAttrs
        (name: value: value.packages."${system}".${name})
        (nixpkgs.lib.genAttrs myFlakes (name: inputs.${name}));
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
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.tim.imports = [ ./home.nix ];
              home-manager.extraSpecialArgs = {
                inherit nixpkgs-unstable;
                inherit nixpkgs;
                inherit flakePkgs;
              };
            }
          ];
        };

        # Run the following command in the flake's directory to
        # deploy this configuration on any NixOS system:
        #   sudo nixos-rebuild switch --flake .#laptop
        laptop = nixpkgs.lib.nixosSystem {
          inherit system;
          inherit specialArgs;
          modules = modules ++ [
            ./systems/laptop/hardware.nix
            ./systems/laptop/hardware-configuration.nix
            ./systems/laptop/services.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.tim.imports = [ ./home.nix ];
              home-manager.extraSpecialArgs = {
                inherit nixpkgs-unstable;
                inherit nixpkgs;
                inherit flakePkgs;
              };
            }
          ];
        };

        # Run the following command in the flake's directory to
        # deploy this configuration on any NixOS system:
        #   sudo nixos-rebuild switch --flake .#tim-mercury
        tim-mercury = nixpkgs.lib.nixosSystem {
          inherit system;
          inherit specialArgs;
          modules = modules ++ [
            ./systems/tim-mercury/hardware.nix
            ./systems/tim-mercury/hardware-configuration.nix
            ./systems/tim-mercury/services.nix
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.tim.imports = [ ./home.nix ];
              home-manager.extraSpecialArgs = {
                inherit nixpkgs-unstable;
                inherit nixpkgs;
                inherit flakePkgs;
              };
            }
          ];
        };
      };
    };
}
