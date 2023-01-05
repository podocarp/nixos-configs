{ pkgs, configDir, ... }:

{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      coc-css
      coc-json
      coc-nvim
      coc-pyright
      coc-snippets
      coc-tsserver
      coc-vimtex
      command-t
      gitsigns-nvim
      nvim-dap
      nvim-dap-ui
      nvim-tree-lua
      nvim-web-devicons
      papercolor-theme
      ultisnips
      undotree
      vifm-vim
      vim-airline
      vim-airline-themes
      vim-fugitive
      vim-nix
      vim-obsession
      vim-sleuth
      vim-surround
      vimtex

      (pkgs.vimPlugins.nvim-treesitter.withPlugins (plugins: [
        plugins.tree-sitter-go
        plugins.tree-sitter-vim
        plugins.tree-sitter-nix
      ]))
    ];

    extraPython3Packages = p: with p; [
      # note: python lsps will use the current interpreter, and so will not see
      # vim extra python packages
      inotify-simple # ultisnips needs this
      unidecode # ultisnips needs this
    ];

    extraPackages = with pkgs; [
      delve # go debugger
      haskell-language-server
      haskellPackages.hlint
      gopls # go language server
      xdotool # for synctex
      ripgrep # for cocsearch
      rnix-lsp
    ];

    # The following symlinks vi, vim, vimdiff to the nvim equivalents.
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    # Unfortunately having the snippets file RO is too troublesome.
    extraConfig = (builtins.readFile ./.vimrc) + ''
      let g:UltiSnipsSnippetDirectories=['${toString ./ultisnips}']
    '';

    coc.enable = true;
    coc.settings = {
      "coc.source.around.enable" = false;
      "coc.preferences.jumpCommand" = "tab drop";
      "coc.preferences.formatOnSaveFiletypes" = [ "*" ];
      "diagnostic.displayByAle" = false;
      "diagnostic.refreshAfterSave"= true;
      "diagnostic.checkCurrentLine" = true;
      "suggest.maxCompleteItemCount" = 10;
      "codeLens" = {
        "enable" = true;
        "separator" = "▸";
        "subseparator" = "▹";
      };
      "python" = {
        "formatting.provider" = "autopep8";
        "linting.flake8Enabled" = true;
      };
      "languageserver" = {
        "haskell" = {
          "command" = "haskell-language-server-wrapper";
          "args" = ["--lsp"];
          "rootPatterns" = ["*.cabal" "stack.yaml" "cabal.project"
          "package.yaml" "hie.yaml"];
          "filetypes" = ["haskell" "lhaskell"];
          "initializationOptions" = {
            "languageServerHaskell" = {
              "hlintOn" = true;
              "completionSnippetsOn" = true;
            };
          };
        };
        "golang" = {
          "command" = "gopls";
          "rootPatterns" = ["go.mod"];
          "filetypes" = ["go"];
          "initializationOptions" = {
            "usePlaceholders" = true;
          };
        };
        "nix" = {
          "command" = "rnix-lsp";
          "filetypes" = ["nix"];
        };
      };
      "snippets.ultisnips.enable" = false;
    };
  };

  home.packages = [ pkgs.neovim-remote ];

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
