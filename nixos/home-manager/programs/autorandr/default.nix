{ ... }:

let
  screen4k = "00ffffffffffff005a63348c*";
  officeScreenH = "00ffffffffffff0010ace*";
  officeScreenV = "00ffffffffffff0010acb*";
  laptopscreen = "0*";
  settingslaptop = {
    enable = true;
    primary = true;
    mode = "1920x1080";
    rate = "60.00";
  };
  settings4k = {
    enable = true;
    primary = false;
    mode = "3840x2160";
    dpi = 120;
    rate = "60.00";
    position = "0x0";
  };
  officeConfig = {
    "DP-1-1" = {
      enable = true;
      primary = true;
      mode = "1920x1080";
      dpi = 100;
      rate = "60.00";
      position = "0x420";
    };
    "DP-1-3" = {
      enable = true;
      primary = false;
      mode = "1920x1080";
      dpi = 100;
      rate = "60.00";
      position = "1920x0";
      rotate = "right";
    };
    "eDP-1" = { enable = false; };
  };
in
{
  programs.autorandr = {
    enable = true;

    profiles = {
      "laptop-only" = {
        fingerprint = {
          "eDP-1" = laptopscreen;
        };
        config = {
          "eDP-1" = settingslaptop;
        };
      };

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
          "DP-1-1" = officeScreenH;
          "DP-1-3" = officeScreenV;
          "eDP-1" = laptopscreen;
        };
        config = officeConfig;
      };

      "office_post" = {
        fingerprint = {
          "DP-1-1" = officeScreenH;
          "DP-1-3" = officeScreenV;
        };
        config = officeConfig;
      };
    };

    hooks = {
      postswitch = {
        "new background" = "systemctl --user restart random-background";
      };
    };
  };
}
