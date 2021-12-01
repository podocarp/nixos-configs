{ pkgs, ... }:

{
  home.file."Scripts" = {
      source = ./.;
  };
}
