{ pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    config = {
      keep-open = "no";
    };
    bindings = {
      WHEEL_UP = "seek 5";
      WHEEL_DOWN = "seek -5";
      LEFT = "seek -5";
      RIGHT = "seek 5";
      Shift+RIGHT = "seek 1 exact"
      Shift+LEFT = "seek -1 exact"
    };
  };
}
