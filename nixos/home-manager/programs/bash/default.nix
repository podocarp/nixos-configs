{ pkgs, ... }:
{
  programs.bash = {
    enable = true;
    sessionVariables = {
      EDITOR = "nvim";
      PATH = "$PATH:~/.scripts:~/Scripts:~/go/bin";
      GPG_TTY = "$(tty)";
      SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
      NIXPKGS_ALLOW_UNFREE = "1";
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
      v = "vim -S";
      sudo = "sudo ";
    };
    initExtra = ''
      stty -ixon
      set -o vi
      HISTCONTROL=ignoreboth

      PS1="\[\033[1;32m\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\$\[\033[0m\] "
    '';
  };
}
