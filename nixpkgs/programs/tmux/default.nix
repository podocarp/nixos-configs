{ pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    escapeTime = 10;
    keyMode = "vi";
    terminal = "screen-256color";
    extraConfig = builtins.readFile ./tmux.conf;
  };
}
