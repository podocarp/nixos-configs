{ pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    escapeTime = 10;
    keyMode = "vi";
    newSession = true;
    terminal = "screen-256color";
    extraConfig = builtins.readFile ./tmux.conf;
    plugins = with pkgs.tmuxPlugins; [
      {
        plugin = resurrect;
        extraConfig = ''
          set -g @resurrect-strategy-nvim 'session'
          set -g @resurrect-capture-pane-contents 'on'
        '';
      }
    ];
  };
}
