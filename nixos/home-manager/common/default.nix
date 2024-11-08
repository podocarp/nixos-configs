args@{ ... }:
{
  imports = [
    ./dev.nix
    ((import ./env.nix) args)
  ];

  programs.home-manager.enable = true;
  home.username = "pengu";
  home.homeDirectory = "/home/pengu";
}
