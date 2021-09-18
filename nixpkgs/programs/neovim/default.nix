{ pkgs, configDir, ... }:

{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      ale
      coc-css
      coc-html
      coc-json
      coc-snippets
      coc-tsserver
      coc-vimtex
      coc-nvim
      command-t
      nerdtree
      papercolor-theme
      ultisnips
      undotree
      vim-airline
      vim-airline-themes
      vim-snippets
      vim-surround
      vimtex
    ];

    extraPython3Packages = p: with p; [
      black
      inotify-simple
      jedi
      pyflakes
      unidecode
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

    extraConfig = builtins.readFile (toString ./.vimrc) + ''
      " Unfortunately having the snippets file RO is too troublesome.
      let g:UltiSnipsSnippetDirectories=[
       \ '${toString ./ultisnips}',
       \ '${pkgs.vimPlugins.vim-snippets}/UltiSnips']
    '';

    coc.enable = true;
    coc.settings = {
      "coc.source.around.enable" = false;
      "diagnostic.displayByAle" = true;
      "diagnostic.refreshAfterSave"= true;
      "suggest.maxCompleteItemCount" = 10;
      "codeLens" = {
        "enable" = true;
        "separator" = "▸";
        "subseparator" = "▹";
      };
      "languageserver"= {
        "haskell"= {
          "command"= "haskell-language-server-wrapper";
          "args"= ["--lsp"];
          "rootPatterns"= ["*.cabal" "stack.yaml" "cabal.project"
            "package.yaml" "hie.yaml"];
          "filetypes"= ["haskell" "lhaskell"];
        };
      };
      "typescript.disableAutomaticTypeAcquisition" = true;
      "typescript" = {
        "disableAutomaticTypeAcquisition"= true;
        "referencesCodeLens.enable"= false;
        "suggest.completeFunctionCals"= false;
        "suggest.includeAutomaticOptionalChainCompletions"= false;
        "format.enabled"= false;
      };
    };
  };

  xdg.configFile."nvim/after/syntax/tex.vim".source = ./syntax/tex.vim;
}
