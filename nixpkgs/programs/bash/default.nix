{ pkgs, ... }:

{
  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "nvim";
      PATH = "$PATH:~/Scripts";
    };
    shellAliases = {
      la = "ls -a";
      lla = "ls -la";
      f = "grep -RHn";
      v ="vim .";
    };
    initExtra = ''
    vf(){
        command vifm "$@"
        if [ -f /tmp/lastdir ]; then
            cd `cat /tmp/lastdir`
        fi
    }
    stty -ixon
    set -o vi
    HISTCONTROL=ignoreboth
    PS1="\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\$\[\033[0m\] "

    if [ -n "$VIFM_SHELL" ]; then
      PS1="[vifm shell]$PS1"
      unset VIFM_SHELL
    fi
    '';
  };
}
