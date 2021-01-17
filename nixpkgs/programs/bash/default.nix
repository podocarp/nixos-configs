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
    stty -ixon
    set -o vi
    HISTCONTROL=ignoreboth
    '';
  };
}
