{ ... }:

{
  programs.readline = {
    enable = true;
    includeSystemConfig = false;
    extraConfig = ''
      set show-mode-in-prompt on
      set vi-ins-mode-string "└╼"
      set vi-cmd-mode-string "└╎"
      set enable-keypad on
      # a single tab will partially complete the word and show all possible
      # completions if it is still ambiguous
      set show-all-if-ambiguous on
      set menu-complete-display-prefix on
      set page-completions off
      # Color files by types
      set colored-stats On
      # Append char to indicate type
      set visible-stats On
      # Mark symlinked directories
      set mark-symlinked-directories On
      # Color the common prefix
      set colored-completion-prefix On
      # Color the common prefix in menu-complete
      set menu-complete-display-prefix On
    '';
  };
}
