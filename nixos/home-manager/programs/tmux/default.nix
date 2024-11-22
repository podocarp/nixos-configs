{ pkgs, ... }:

{
  programs.tmux = {
    baseIndex = 1;
    disableConfirmationPrompt = true;
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
        # https://discourse.nixos.org/t/how-to-get-tmux-resurrect-to-restore-neovim-sessions/30819/11
        extraConfig = ''
          resurrect_dir="$HOME/.tmux/resurrect"
          set -g @resurrect-dir $resurrect_dir
          set -g @resurrect-hook-post-save-all "sed -i 's/--cmd lua.*--cmd set packpath/--cmd \"lua/g; s/--cmd set rtp.*\$/\"/' $resurrect_dir/last"
          set -g @resurrect-capture-pane-contents 'on'
          set -g @resurrect-processes '"~nvim"'
        '';
      }
    ];
  };
}
