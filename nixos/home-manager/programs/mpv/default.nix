{ pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    config = {
      keep-open = "no";
    };
  };
}
