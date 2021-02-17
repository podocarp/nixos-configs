# This file includes things related to X and stuff on X.
{ pkgs, ... }:

{
  services.xserver = {
    enable = true;
    xkbOptions = "caps:escape"; # this maps caps to escape.

    libinput = {
      enable = true;
      touchpad.scrollButton = 2;
    };

    # This starts ~/.xsession, which allows home-manager to control some things.
    displayManager.session = [
      {
        name = "xsession";
        start = "${pkgs.runtimeShell} $HOME/.xsession & waitPID=$!";
        manage = "window";
      }
    ];

    useGlamor = true;
  };

  # This enables the changing of gtk themes by home-manager.
  programs.dconf.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  fonts.fontconfig.enable = true;
}
