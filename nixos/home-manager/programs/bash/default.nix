{ pkgs, ... }:

{
  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "nvim";
      PATH = "$PATH:~/Scripts";
      GPG_TTY = "$(tty)";
      SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    };
    shellAliases = {
      la = "ls -a";
      lla = "ls -la";
      f = "grep -RHn";
      v ="vim .";
    };
    initExtra = ''
        stty -ixon
        set -o vi
        HISTCONTROL=ignoreboth

        if [ -x "$(command -v tmux)" ] && [ -n "''${DISPLAY}" ]; then
          [ -z "''${TMUX}" ] && { tmux; } >/dev/null 2>&1
        fi

        PS1="\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\$\[\033[0m\] "
    '';
  };
}
