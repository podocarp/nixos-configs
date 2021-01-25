{ ... }:

{
  programs.readline = {
    enable = true;
    extraConfig = ''
      set show-mode-in-prompt on
      set vi-ins-mode-string "└╼"
      set vi-cmd-mode-string "└╎"
      set enable-keypad on
      TAB: menu-complete
      "\e[Z": menu-complete-backward
      set show-all-if-unmodified on
      set show-all-if-ambiguous on
      set menu-complete-display-prefix on
      set page-completions off
    '';
  };
}
