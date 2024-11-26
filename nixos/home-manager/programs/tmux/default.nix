{ pkgs, ... }:

{
  programs.tmux = {
    baseIndex = 1;
    disableConfirmationPrompt = false;
    enable = true;
    escapeTime = 10;
    keyMode = "vi";
    mouse = true;
    newSession = false;
    terminal = "screen-256color";
    extraConfig = builtins.readFile ./tmux.conf;
    plugins = with pkgs.tmuxPlugins; [
      {
        plugin = resurrect;
        # Resurrect saves the wrong command to start nvim. With nixvim, the
        # nvim command is actually a script that sets up a lot of runtime paths
        # etc so we can't run the nvim binary directly.
        extraConfig = ''
          set -g @resurrect-dir $HOME/.tmux/resurrect
          set -g @resurrect-hook-post-save-all 'sed "s|:/nix/store/.*/bin/nvim.*|:nvim|" $HOME/.tmux/resurrect/last | tee $HOME/.tmux/resurrect/last > /dev/null'
          set -g @resurrect-capture-pane-contents 'on'
        '';
      }
    ];
  };
}
