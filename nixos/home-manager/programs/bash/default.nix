{ ... }:
{
  programs.bash = {
    enable = true;
    enableCompletion = true;
    sessionVariables = {
      EDITOR = "nvim";
      GPG_TTY = "$(tty)";
      HISTCONTROL = "ignoreboth";
      NIXPKGS_ALLOW_INSECURE = "1";
      NIXPKGS_ALLOW_UNFREE = "1";
      PATH = "$PATH:~/.scripts:~/.npm-global/bin:~/go/bin";
    };
    historyControl = [ "ignoredups" ];
    historyIgnore = [
      "exit"
      "ls"
      "cd"
      "lla"
      "la"
    ];
    shellAliases = {
      escapeWhitespace = "sed -e 's/\\\\n/\\n/g' -e 's/\\\\t/\\t/g'";
      f = "grep -RHn";
      g = "git";
      la = "ls -a";
      lla = "ls -la";
      t = "tmux -u";
      v = "nvim";
      # The trailing space makes the shell try alias expansion on the next word
      sudo = "sudo ";
    };
    initExtra = ''
      stty -ixon
      set -o vi

      PS1="\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\$\[\033[0m\] "

      bind 'TAB: menu-complete'
      bind -m vi-command ".":insert-last-argument
      bind -m vi-insert "\C-l.":clear-screen
      bind -m vi-insert "\C-a.":beginning-of-line
      bind -m vi-insert "\C-e.":end-of-line
      bind -m vi-insert "\C-w.":backward-kill-word
    '';
  };
}
