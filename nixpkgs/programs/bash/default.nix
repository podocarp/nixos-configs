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
    '';
  };
}
