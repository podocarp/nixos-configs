{ pkgs, ... }:

{
  programs.mpv = {
    enable = true;
    config = {
      "keep-open" = "no";
      "osd-font-size" = "25";
      "screenshot-format" = "png";
      "volume-max" = "150";
      #"hwdec" = "auto-safe";
      #"vo" = "gpu";
      #"profile" = "gpu-hq";
    };
    bindings = {
      "WHEEL_UP" = "seek 5";
      "WHEEL_DOWN" = "seek -5";
      "LEFT" = "seek -5 exact";
      "RIGHT" = "seek 5 exact";
      "Shift+RIGHT" = "seek 1 exact";
      "Shift+LEFT" = "seek -1 exact";
      "F11" = "cycle fullscreen";
      "F5" = "screenshot";
      "s" = "cycle sub";
      "a" = "cycle audio";
    };
  };
}
