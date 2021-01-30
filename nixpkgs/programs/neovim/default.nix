{pkgs, configDir, ...}:

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
        coc-vimtex
        command-t
        gruvbox
        nerdtree
        ultisnips
        vim-airline
        vim-snippets
        vim-surround
        vimtex
        ];

    extraPackages = with pkgs; [
      python38Packages.jedi
      python38Packages.pylint
    ];

    # The following symlinks vi, vim, vimdiff to the nvim equivalents.
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython = false;
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
