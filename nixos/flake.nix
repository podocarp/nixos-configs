{
  description = "NixOS configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = inputs@{ nixpkgs, home-manager, sops-nix, ... }: {
    nixosConfigurations = {
      work = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          ./machines/work-laptop.nix
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
        ];
      };

      server = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = inputs;
        modules = [
          ./machines/server.nix
          home-manager.nixosModules.home-manager
          sops-nix.nixosModules.sops
        ];
      };
    };
  };
}
