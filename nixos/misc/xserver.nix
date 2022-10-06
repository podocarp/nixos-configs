{ pkgs, ... }:
{
  imports = [
    ./fcitx.nix
  ];

  services.xserver = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      config = ./xmonad/xmonad.hs;
      enableConfiguredRecompile = true;
      enableContribAndExtras = true;
    };

    desktopManager.plasma5 = {
      enable = true;
      runUsingSystemd = false;
    };

    # This starts ~/.xsession, which allows home-manager to control some things.
    displayManager = {
      session = [
        {
          name = "xsession";
          start = "${pkgs.runtimeShell} $HOME/.xsession-hm & waitPID=$!";
          manage = "window";
        }
      ];
      defaultSession = "none+xsession";
      autoLogin = {
        enable = true;
        user = "pengu";
      };
    };
  };

  # Allows HM gnome configs.
  programs.dconf.enable = false;
}
