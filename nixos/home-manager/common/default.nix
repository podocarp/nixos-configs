{ ... }:
{
  imports = [
    ./dev.nix
    ./env.nix
  ];

  programs.home-manager.enable = true;
  home.username = "pengu";
  home.homeDirectory = "/home/pengu";
}
