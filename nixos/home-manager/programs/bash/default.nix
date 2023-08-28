{ pkgs, ... }:
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
      PATH = "$PATH:~/.scripts:~/Scripts:~/go/bin";
    };
    historyControl = [ "ignoredups" ];
    historyIgnore = [ "exit" "ls" "cd" "lla" "la" ];
    shellAliases = {
      escapeWhitespace = "sed -e 's/\\\\n/\\n/g' -e 's/\\\\t/\\t/g'";
      f = "grep -RHn";
      g = "git";
      la = "ls -a";
      lla = "ls -la";
      t = "tmux -u";
      v = "nvim -S";
      # The trailing space makes the shell try alias expansion on the next word
      sudo = "sudo ";
    };
    initExtra = ''
      stty -ixon
      set -o vi

      if [ "''${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
        export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
      fi

      PS1="\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\$\[\033[0m\] "

      bind 'TAB: menu-complete'
    '';
  };
}
