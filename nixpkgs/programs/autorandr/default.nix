{ ... }:

let
    screen4k = "00ffffffffffff005a63348c15010101*";
    screen1080left = "00ffffffffffff004c2d2c0d47385030*";
    laptopscreen = "0*";
    settingslaptop = {
      enable = true;
      crtc = 0;
      primary = true;
      mode = "1920x1080";
      dpi = 110;
      rate = "60.00";
    };
    settings4k = {
      enable = true;
      primary = true;
      crtc = 0;
      dpi = 140;
      mode = "3840x2160";
      rate = "60.00";
    };
    settings4koffset = {
      enable = true;
      primary = true;
      crtc = 2;
      dpi = 140;
      mode = "3840x2160";
      position = "1920x0";
      rate = "60.00";
    };
    settings1080left = {
      enable = true;
      crtc = 0;
      mode = "1920x1080";
      position = "0x0";
      rate = "60.00";
      rotate = "left";
    };
in
{
  programs.autorandr = {
    enable = true;

    profiles."solo-4k" = {
      fingerprint = {
        "DP-1.3" = screen4k;
        "eDP-1-1" = laptopscreen;
      };
      config = {
        "DP-0".enable = false;
        "DP-1".enable = false;
        "HDMI-0".enable = false;
        "eDP-1-1".enable = false;
        "DP-1.3" = settings4k;
      };
    };

    profiles."4k-1080left" = {
      fingerprint = {
        "DP-1.1" = screen1080left;
        "DP-1.3" = screen4k;
        "eDP-1-1" = laptopscreen;
      };
      config = {
        "DP-0".enable = false;
        "DP-1".enable = false;
        "eDP-1-1".enable = false;
        "HDMI-0".enable = false;
        "DP-1.1" = settings1080left;
        "DP-1.3" = settings4koffset;
      };
      # Autorandr insists on setting fb which xrandr does not like since our
      # monitors are rotated.
      hooks.postswitch = ''
        xrandr --output DP-1.3 --pos 1080x0
      '';
    };

    # Single laptop display on hybrid graphics
    profiles."laptop-only" = {
      fingerprint = {
        "eDP-1-1" = laptopscreen;
      };
      config = {
        "eDP-1-1" = settingslaptop;
      };
    };
  };
}
