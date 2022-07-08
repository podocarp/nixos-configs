{ ... }:

let
  screen4k = "00ffffffffffff005a63348c*";
  officeScreen = "00ffffffffffff0010ac*";
  laptopscreen = "0*";
  settingslaptop = {
    enable = true;
    primary = true;
    mode = "1920x1080";
    rate = "60.00";
  };
  settingsOffice = {
    enable = true;
    primary = false; mode = "1920x1080";
    dpi = 100;
    rate = "60.00";
    position = "0x0";
  };
  settings4k = {
    enable = true;
    primary = false;
    mode = "3840x2160";
    dpi = 120;
    rate = "60.00";
    position = "0x0";
  };
in
{
  programs.autorandr = {
    enable = true;

    profiles = {
      "work-from-home" = {
        fingerprint = {
          "eDP-1" = laptopscreen;
          "HDMI-1" = screen4k;
        };
        config = {
          "eDP-1" = settingslaptop // {position = "960x2160";};
          "HDMI-1" = settings4k;
        };
      };

      "office" = {
        fingerprint = {
          "eDP-1" = laptopscreen;
          "DP-2" = officeScreen;
        };
        config = {
          "eDP-1" = settingslaptop // {position = "0x1080";};
          "DP-2" = settingsOffice;
        };
      };
    };

    hooks = {
      postswitch = {
        "restart xmonad" = "xmonad --restart";
        "new background" = "systemctl --user restart random-background";
      };
    };
  };
}
