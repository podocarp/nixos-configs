{ ... }:

{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = ./xmonad.hs;
  };

  xdg.configFile."xmobar/xmobarrc".source = ./xmobarrc;

  programs.xmobar = {
    enable = true;
  };
}