{ pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    escapeTime = 10;
    keyMode = "vi";
    newSession = true;
    terminal = "screen-256color";
    extraConfig = builtins.readFile ./tmux.conf;
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = "set -g @resurrect-strategy-nvim 'session'";
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
        '';
      }
    ];
  };
}
