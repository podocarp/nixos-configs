{ pkgs, ... }:
{
  imports = [
    ./fcitx.nix
  ];

  programs.dconf.enable = true; # needed for all kinds of gnome things

  services.xserver = {
    enable = true;

    # stops tearing?
    screenSection = ''
      Option "metamodes" "nvidia-auto-select +0+0 { ForceCompositionPipeline = On }"
    '';

    desktopManager.runXdgAutostartIfNone = true;

    # This starts ~/.xsession, which allows home-manager to control some things.
    displayManager = {
      session = [
        {
          name = "xsession";
          start = "${pkgs.runtimeShell} $HOME/.xsession-hm & waitPID=$!";
          manage = "window";
        }
      ];
    };
  };

  services.displayManager = {
    defaultSession = "none+xsession";
    autoLogin = {
      enable = true;
      user = "pengu";
    };
  };
}
