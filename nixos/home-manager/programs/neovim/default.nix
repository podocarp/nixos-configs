{ pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins;
      [
        coc-css
        coc-eslint
        coc-go
        coc-json
        coc-nvim
        coc-prettier
        coc-pyright
        coc-ultisnips
        coc-tsserver
        coc-vimtex
        fzf-vim
        gitsigns-nvim
        nvim-dap
        nvim-dap-ui
        nvim-dap-python
        nvim-dap-go
        nvim-tree-lua
        nvim-web-devicons
        papercolor-theme
        ultisnips
        undotree
        vim-airline
        vim-airline-themes
        vim-fugitive
        vim-obsession
        vim-sleuth
        vim-surround
        vimtex

        (nvim-treesitter.withPlugins (plugins: [
          plugins.tree-sitter-c
          plugins.tree-sitter-go
          plugins.tree-sitter-lua
          plugins.tree-sitter-nix
          plugins.tree-sitter-vim
        ]))
        nvim-treesitter-textobjects
      ];

    extraPython3Packages = p: with p; [
      # note: python lsps will use the current interpreter, and so will not see
      # vim extra python packages
      inotify-simple # ultisnips needs this
      unidecode # ultisnips needs this
      black
      mypy
      flake8
    ];

    extraPackages = with pkgs; [
      nil # nix lsp
      nixpkgs-fmt
      delve # go debugger
      gopls # go language server
      gofumpt # go fmt replacement
      haskell-language-server
      haskellPackages.hlint
      ripgrep # for cocsearch
      tailwindcss-language-server
      tree-sitter
      watchman # for coc
    ];

    # The following symlinks vi, vim, vimdiff to the nvim equivalents.
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    coc.enable = true;
    # can override in local dir's .vim/coc-settings.json
    coc.settings = {
      "coc.source.around.enable" = false;
      "coc.preferences.jumpCommand" = "tab drop";
      "coc.preferences.formatOnSaveFiletypes" = [ "*" ];
      "diagnostic.displayByAle" = false;
      "diagnostic.refreshAfterSave" = true;
      "diagnostic.checkCurrentLine" = true;
      "suggest.maxCompleteItemCount" = 10;
      "codeLens" = {
        "enable" = true;
        "separator" = "▸";
        "subseparator" = "▹";
      };

      # the pyright plugin doesn't like nested json for some reason...
      "python.pythonPath" = "nvim-python3";
      "python.formatting.provider" = "black";
      "python.linting.flake8Enabled" = true;
      "python.linting.mypyEnabled" = true;

      "typescript" = {
        "format" = {
          "semicolons" = "insert";
          "insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces" = true;
        };
        "preferences" = {
          "importModuleSpecifier" = "project-relative";
          "quoteStyle" = "single";
        };
      };
      "go" = {
        goplsEnv = {
          "GOFUMPT_SPLIT_LONG_LINES" = "on";
        };
        "goplsOptions" = {
          templateExtensions = [ "go.tmpl" "tmpl" ];
          gofumpt = true;
        };
        "goplsPath" = "${pkgs.gopls}/bin/gopls";
      };

      "tsserver.useLocalTsdk" = true;
      "languageserver" = {
        "haskell" = {
          "command" = "haskell-language-server-wrapper";
          "args" = [ "--lsp" ];
          "filetypes" = [ "haskell" "lhaskell" ];
          "rootPatterns" = [
            "*.cabal"
            "stack.yaml"
            "cabal.project"
            "package.yaml"
            "hie.yaml"
          ];
        };
        "nix" = {
          "command" = "nil";
          "filetypes" = [ "nix" ];
          "rootPatterns" = [ "flake.nix" ];
          "settings" = {
            "nil" = {
              "formatting" = {
                "command" = [ "nixpkgs-fmt" ];
              };
            };
          };
        };
        "tailwindcss" = {
          "command" = "tailwindcss-language-server";
          "args" = [ "--stdio" ];
          "filetypes" = [ "javascriptreact" "typescriptreact" ];
          "rootPatterns" = [ "tailwind.config.js" "tailwind.config.ts" ];
        };
      };
      "snippets.ultisnips.enable" = true;
    };
  };

  home.packages = [ pkgs.neovim-remote ];

  xdg.configFile.nvim = {
    source = ./configs;
    recursive = true;
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

