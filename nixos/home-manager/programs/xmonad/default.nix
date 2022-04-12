{ pkgs, myTerm, myBorderWidth ? 3, ... }:

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
        myBorderWidth = ${toString myBorderWidth}
        ''
      );
  };

  programs.xmobar = {
    enable = true;
    extraConfig = builtins.readFile ./xmobarrc;
  };
}
