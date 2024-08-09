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
          myTerm = "xterm"
          myBorderWidth = 3
        ''
      );
  };

  xdg.configFile."xmobar/xmobarrc".source = ./xmobarrc;
  xdg.configFile."xmobar/xmobarrc_unfocused".source = ./xmobarrc_unfocused;

  programs.xmobar = {
    enable = true;
  };
}
