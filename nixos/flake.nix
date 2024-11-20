{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    sops-nix.url = "github:Mic92/sops-nix";

    flake-utils.url = "github:numtide/flake-utils";

    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      nixpkgs,
      home-manager,
      sops-nix,
      nix-darwin,
      ...
    }:
    {
      nixosConfigurations =
        let
          registryPin = (
            { ... }:
            {
              nix.registry = {
                nixpkgs.flake = nixpkgs;
              };
            }
          );
        in
        {
          server = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = inputs;
            modules = [
              ./machines/server.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
              registryPin
            ];
          };

          server-min = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = inputs;
            modules = [
              ./machines/server_min.nix
              home-manager.nixosModules.home-manager
            ];
          };

          desktop = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = inputs;
            modules = [
              ./machines/desktop.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
              registryPin
            ];
          };

          t420 = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = inputs;
            modules = [
              ./machines/t420.nix
              home-manager.nixosModules.home-manager
              sops-nix.nixosModules.sops
              registryPin
            ];
          };

          x1 = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = inputs;
            modules = [
              ./machines/x1-extreme.nix
              home-manager.nixosModules.home-manager
              {
                home-manager.extraSpecialArgs = {
                  inherit inputs;
                };
              }
              sops-nix.nixosModules.sops
              registryPin
            ];
          };
        };

      darwinConfigurations = {
        system = "aarch64-darwin";
        work = nix-darwin.lib.darwinSystem {
          specialArgs = inputs;
          modules = [
            ./machines/work.nix
            # sops does not have a darwin module yet
            home-manager.darwinModules.home-manager
            {
              home-manager.extraSpecialArgs = {
                inherit inputs;
              };
            }
          ];
        };

        jasmine = nix-darwin.lib.darwinSystem {
          specialArgs = inputs;
          modules = [
            ./machines/jasmine.nix
            home-manager.darwinModules.home-manager
          ];
        };
      };

      legacyPackages.x86_64-linux.default = nixpkgs.legacyPackages.x86_64-linux;

      devShell =
        let
          shell =
            { pkgs }:
            pkgs.mkShell {
              nativeBuildInputs = with pkgs; [
                (haskellPackages.ghcWithPackages (hp: [
                  hp.xmonad
                  hp.xmonad-contrib
                  hp.regex-posix
                ]))
                sops
                age
                wireguard-tools
              ];
              shellHook = '''';
            };
        in
        {
          x86_64-linux = shell {
            pkgs = nixpkgs.legacyPackages.x86_64-linux;
          };
          aarch64-linux = shell {
            pkgs = nixpkgs.legacyPackages.aarch64-linux;
          };
          aarch64-darwin = shell {
            pkgs = nixpkgs.legacyPackages.aarch64-darwin;
          };
        };
    };
}
