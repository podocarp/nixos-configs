# This file includes things related to X and stuff on X.
{ pkgs, ... }:

{
  services.xserver = {
    enable = true;
    xkbOptions = "caps:escape,keypad:pointerkeys"; # this maps caps to escape.

    desktopManager.plasma5.enable = true;

    # This starts ~/.xsession, which allows home-manager to control some things.
    displayManager.session = [
      {
        name = "xsession";
        start = "${pkgs.runtimeShell} $HOME/.xsession-hm & waitPID=$!";
        manage = "window";
      }
    ];

    useGlamor = true;
    # wacom.enable = true;
  };

  console.useXkbConfig = true;
}
