{ ... }:

let
    screen4k =
"00ffffffffffff005a63348c150101011b1e0104b54627783f6875a6564fa2260e5054bfef80d100d1c0b300a94095008180810081c022cc0050f0703e8018103500b9882100001a000000ff005630453230323730303035380a000000fd00184b0f873c000a202020202020000000fc005650333236382d344b0a20202001db02032bf15e61605f5e5d6b666501020304101f05140607080911121315161d1e2021222309070783010000a36600a0f0701f8030203500b9882100001a565e00a0a0a0295030203500b9882100001a1a6800a0f0381f4030203a00b9882100001a4d6c80a070703e8030203a00b9882100001a00000000000000000000000060";
    screen1080left =
"00ffffffffffff004c2d2c0d473850302e19010380341d782a5295a556549d250e5054bb8c00b30081c0810081809500a9c001010101023a801871382d40582c450009252100001e000000fd0032481e5111000a202020202020000000fc00433234463339300a2020202020000000ff00485451484330303236340a2020015e02031af14690041f131203230907078301000066030c00100080011d00bc52d01e20b828554009252100001e8c0ad090204031200c4055000925210000188c0ad08a20e02d10103e96000925210000180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c9";
    laptopscreen = "00ffffffffffff000daee81500000000211a0104a5221378022675a6565099270c505400000001010101010101010101010101010101b43b804a713834405036680058c110000018000000fe004e3135364843452d454e310a20000000fe00434d4e0a202020202020202020000000fe004e3135364843452d454e310a2000a2";
    settingslaptop = {
      enable = true;
      crtc = 0;
      primary = true;
      mode = "1920x1080";
      rate = "60.00";
    };
    settings4k = {
      enable = true;
      primary = true;
      crtc = 0;
      mode = "3840x2160";
      rate = "60.00";
    };
    settings4koffset = {
      enable = true;
      primary = true;
      crtc = 2;
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

    # This is a profile on hybrid graphics and docked with a single 4k monitor
    # attached. The laptop screen is disabled.
    profiles."hybrid-solo-4k" = {
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

    # This is a profile on discrete graphics and docked with a single 4k
    # monitor atttached. The laptop screen is disabled.
    profiles."nvidia-solo-4k" = {
      fingerprint = {
        "DP-1.3" = screen4k;
        "DP-2" = laptopscreen;
      };
      config = {
        "DP-0".enable = false;
        "DP-1".enable = false;
        "DP-2".enable = false;
        "HDMI-0".enable = false;
        "DP-1.3" = settings4k;
      };
    };

    # This is a profile on discrete graphics and docked with a single 4k
    # monitor and a vertical 1080p monitor on the left.
    profiles."nvidia-4k-1080left" = {
      fingerprint = {
        "DP-1.1" = screen1080left;
        "DP-1.3" = screen4k;
        "DP-2" = laptopscreen;
      };
      config = {
        "DP-0".enable = false;
        "DP-1".enable = false;
        "DP-2".enable = false;
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
    profiles."hybrid-laptop-only" = {
      fingerprint = {
        "eDP-1-1" = laptopscreen;
      };
      config = {
        "eDP-1-1" = settingslaptop;
      };
    };

    # Single laptop display on discrete graphics (docked)
    profiles."discrete-laptop-only" = {
      fingerprint = {
        "DP-2" = laptopscreen;
      };
      config = {
        "DP-2" = settingslaptop;
      };
    };
  };
}
