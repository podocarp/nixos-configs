{ pkgs, ... }:
{
  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "nvim";
      PATH = "$PATH:~/Scripts:~/go/bin";
      GPG_TTY = "$(tty)";
      SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    };
    historyControl = [ "ignoredups" ];
    historyIgnore = [ "exit" "ls" "cd" "lla" "la" ];
    shellAliases = {
      la = "ls -a";
      lla = "ls -la";
      f = "grep -RHn";
      v = "vim -S";
      t = "tmux -u";
    };
    initExtra = ''
        stty -ixon
        set -o vi
        HISTCONTROL=ignoreboth

        PS1="\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\$\[\033[0m\] "
    '';
  };
}
