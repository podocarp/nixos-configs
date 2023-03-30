{ pkgs, ... }:

{
  home.file.".scripts" = {
    source = ./.;
    executable = true;
  };
}
