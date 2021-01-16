{pkgs, configDir, ...}:

{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      ale
      coc-css coc-html coc-json coc-nvim coc-python coc-vimtex
      command-t
      gruvbox
      haskell-vim
      nerdtree
      ultisnips
      vim-airline
      vim-snippets
      vim-surround
      vimtex
    ];

    # The following symlinks vi, vim, vimdiff to the nvim equivalents.
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython = false;
    withPython3 = true;
    withRuby = true;

    extraConfig = builtins.readFile ./.vimrc;
  };
}
