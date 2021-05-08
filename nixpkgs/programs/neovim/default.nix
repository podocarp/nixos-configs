{ pkgs, configDir, ... }:

{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
        ale
        coc-css
        coc-html
        coc-json
        coc-nvim
        coc-snippets
        coc-tsserver
        coc-vimtex
        command-t
        nerdtree
        papercolor-theme
        ultisnips
        undotree
        vim-airline
        vim-airline-themes
        vim-nix
        vim-snippets
        vim-surround
        vimtex
        ];

    extraPython3Packages = p: with p; [
      black
      inotify-simple
      jedi
      pyflakes
    ];

    extraPackages = with pkgs; [
      haskellPackages.hlint

      nodePackages.eslint
      nodePackages.prettier
      nodePackages.typescript
    ];

    # The following symlinks vi, vim, vimdiff to the nvim equivalents.
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    extraConfig = builtins.readFile ./.vimrc + ''
      " Unfortunately having the snippets file RO is too troublesome.
      let g:UltiSnipsSnippetDirectories=[
       \ $HOME.'/.config/nixpkgs/programs/neovim/ultisnips',
       \ '${pkgs.vimPlugins.vim-snippets}/share/vim-plugins/vim-snippets/UltiSnips']
    '';
  };
  xdg.configFile."nvim/coc-settings.json".source = ./coc.json;
}
