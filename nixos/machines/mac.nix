{ config, pkgs, lib, ... }:
{
  imports = [
  ];

  users.users.bytedance = {
    home = "/Users/bytedance";
  };

  home-manager.users.bytedance = import ../home-manager/mac.nix;

  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;
  nix.useDaemon = true;
}
