{ ... }:

{
  programs.tmux = {
    enable = true;
    escapeTime = 10;
    keyMode = "vi";
    newSession = true;
    terminal = "screen-256color";
    extraConfig = builtins.readFile ./tmux.conf;
  };
}
