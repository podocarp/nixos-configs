{ pkgs, ... }:

{
  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "nvim";
    };
    shellAliases = {
      la = "ls -a";
      lla = "ls -la";
      f = "grep -RHn";
      v ="vim .";
      vf = "vifm";
    };
    initExtra = ''
    stty -ixon
    set -o vi
    HISTCONTROL=ignoreboth
    '';
  };
}
