{ pkgs, ... }:
{
  home.stateVersion = "22.11";

  imports = [
    ./common/dev.nix
    ./common/env.nix

    ./programs/htop
  ];

  home.packages = with pkgs; [
    stress
    sysstat
  ];
}

