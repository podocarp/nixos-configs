args@{ pkgs, ... }:
{
  imports = [
    ((import ../services/yabai) args)
    ../services/skhd
  ];

  users.users.jxd = {
    home = "/Users/jxd";
  };

  home-manager.users.jxd = import ../home-manager/jasmine.nix;

  environment.variables = {
    LC_ALL = "en_US.UTF-8";
    LANG = "en_US.UTF-8";
  };

  nixpkgs.hostPlatform = "aarch64-darwin";
  nixpkgs.config.allowUnfree = true;
  nix.useDaemon = true;

  nix = {
    settings = {
      experimental-features = [ "flakes" "nix-command" ];
    };
  };
}
