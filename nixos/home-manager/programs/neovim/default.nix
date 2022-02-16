{ pkgs, configDir, ... }:

{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      # ale
      coc-css
      coc-html
      coc-json
      coc-nvim
      coc-pyright
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
      vim-snippets
      vim-surround
      vimtex
    ];

    extraPython3Packages = p: with p; [
      autopep8
      black
      flake8
      inotify-simple # ultisnips needs this
      unidecode # ultisnips needs this
    ];

    extraPackages = with pkgs; [
      haskell-language-server
      haskellPackages.hlint

      nodePackages.eslint
      nodePackages.prettier
      nodePackages.typescript

      pplatex
      texlab
      xclip
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
      "diagnostic.displayByAle" = false;
      "diagnostic.refreshAfterSave"= true;
      "suggest.maxCompleteItemCount" = 10;
      "codeLens" = {
        "enable" = true;
        "separator" = "▸";
        "subseparator" = "▹";
      };
      "python" = {
          "linting.flake8Enabled" = true;
      };
      "languageserver" = {
        "haskell" = {
          "command" = "haskell-language-server-wrapper";
          "args" = ["--lsp"];
          "rootPatterns" = ["*.cabal" "stack.yaml" "cabal.project"
            "package.yaml" "hie.yaml"];
          "filetypes"= ["haskell" "lhaskell"];
          "initializationOptions" = {
            "languageServerHaskell" = {
              "hlintOn" = true;
              "completionSnippetsOn" = true;
            };
          };
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

  # Used for a snippet.
  xdg.configFile.inkscapeTemplate = {
    source = builtins.fetchurl {
      url = "https://raw.githubusercontent.com/podocarp/riceDumpling/master/.config/inkscape/templates/default1024.svg";
      sha256 = "1kyasmdv02ylk6qz3kxv46rp3czxv5ss07cs91dacadpy4l1dfrq";
    };
    target = "inkscape/templates/default1024.svg";
  };

  # custom tex conceal
  # xdg.configFile."nvim/after/syntax/tex.vim".source = ./syntax/tex.vim;
}
