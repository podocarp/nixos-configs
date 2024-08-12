{ pkgs, ... }:

{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad.hs;
  };

  xdg.configFile."xmobar/xmobarrc".source = ./xmobarrc;

  home.packages = [ pkgs.alsa-utils ];

  programs.xmobar = {
    enable = true;
  };
}
