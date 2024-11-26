args@{ ... }:
{
  imports = [
    ./dev.nix
    ((import ./env.nix) args)
  ];

  programs.home-manager.enable = true;
  home.username = "pengu";
  home.homeDirectory = "/home/pengu";

  home.sessionVariables = {
    NIXPKGS_ALLOW_INSECURE = "1";
    NIXPKGS_ALLOW_UNFREE = "1";
    EDITOR = "nvim";
  };
}
