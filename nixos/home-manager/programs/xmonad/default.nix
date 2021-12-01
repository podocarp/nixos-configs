{ pkgs, myTerm, ... }:

{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = pkgs.writeText
      "xmonad.hs"
      (
        (builtins.readFile ./xmonad.hs) +
        ''
        myTerm = "${myTerm}"
        ''
      );
  };
  xdg.configFile."xmobar/xmobarrc".source = ./xmobarrc;
}
