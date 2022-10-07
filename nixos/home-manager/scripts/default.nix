{ pkgs, ... }:

{
  home.file."Scripts" = {
      source = ./.;
      executable = true;
  };
}
