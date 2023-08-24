{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";

    flake-utils.url = "github:numtide/flake-utils";

    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{ nixpkgs
    , home-manager
    , sops-nix
    , flake-utils
    , nix-darwin
    , ...
    }: {
      nixosConfigurations = {
        server = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [
            ./machines/server.nix
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
          ];
        };

        desktop = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = inputs;
          modules = [
            ./machines/desktop.nix
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
          ];
        };

        orbstack = nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          specialArgs = inputs;
          modules = [
            ./machines/orbstack.nix
            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops
          ];
        };
      };

      darwinConfigurations = {
        system = "aarch64-darwin";
        mac = nix-darwin.lib.darwinSystem {
          specialArgs = inputs;
          modules = [
            ./machines/mac.nix
            home-manager.darwinModules.home-manager
          ];
        };
      };

      legacyPackages.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux;

      devShell.x86_64-linux = import ./shell.nix {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
      };
      devShell.aarch64-linux = import ./shell.nix {
        pkgs = nixpkgs.legacyPackages.aarch64-linux;
      };
      devShell.aarch64-darwin = import ./shell.nix {
        pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      };
    };
}
