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
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
    ];
  };
}
