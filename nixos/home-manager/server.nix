args@{ pkgs, ... }:
{
  home.stateVersion = "22.11";

  imports = [
    (import ./common (args // {
      useX = false;
    }))

    ./programs/htop
  ];

  home.packages = with pkgs; [
    stress
    sysstat
  ];
}

