{pkgs, ...}:

{
  programs.urxvt = {
    enable = true;
    fonts = [ "xft: Dina:size=10:antialias=false" ];
    iso14755 = false;
    # can't figure out how to get perls. not working.
    extraConfig = {
        "perl-ext" = "default, url-select, keyboard-select";
    };
    keybindings = {
      "M-escape" = "perl:keyboard-select:activate";
      "M-u" = "perl:url-select:select-next";
      "C-up" = "font-size:increase";
      "C-down" = "font-size:decrease";
    };
    scroll.bar.enable = false;
  };
}
