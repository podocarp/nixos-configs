{ pkgs, configDir, ... }:

{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      coc-css
      coc-go
      coc-json
      coc-nvim
      coc-pyright
      coc-snippets
      coc-vimtex
      nerdtree
      papercolor-theme
      ultisnips
      undotree
      vifm-vim
      vim-airline
      vim-airline-themes
      vim-nix
      vim-surround
      vim-sleuth
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
      xdotool
    ];

    # The following symlinks vi, vim, vimdiff to the nvim equivalents.
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = false;

    # Unfortunately having the snippets file RO is too troublesome.
    extraConfig = builtins.readFile (toString ./.vimrc) + ''
      let g:UltiSnipsSnippetDirectories=['${toString ./ultisnips}']
    '';

    coc.enable = true;
    coc.settings = {
      "coc.source.around.enable" = false;
      "coc.preferences.jumpCommand" = "tab drop";
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
