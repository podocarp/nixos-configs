{ pkgs, osConfig, ... }:
let
  hostname = osConfig.networking.hostName;
in
{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad.hs;
  };

  xdg.configFile."xmobar/xmobarrc".source = {
    "t420" = ./xmobarrc;
    "x1" = ./xmobarrc;
    "desktop" = ./xmobarrc_desktop;
  }."${hostname}";

  home.packages = [ pkgs.alsa-utils ];

  programs.xmobar = {
    enable = true;
  };
}


