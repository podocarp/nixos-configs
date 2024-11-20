{ pkgs, ... }:

{
  programs.neovim = {
    enable = false;
    plugins = with pkgs.vimPlugins;
      [
        coc-css
        coc-eslint
        coc-go
        coc-json
        coc-nvim
        coc-prettier
        coc-pyright
        coc-snippets
        coc-tsserver
        coc-ultisnips
        coc-vimtex

        fzf-vim

        gitsigns-nvim

        lualine-nvim

        nvim-dap
        nvim-dap-go
        nvim-dap-python
        nvim-dap-ui

        nvim-tree-lua

        nvim-web-devicons

        papercolor-theme

        ultisnips

        undotree

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
          plugins.tree-sitter-latex
        ]))
        nvim-treesitter-textobjects
      ];

    extraPython3Packages = p: with p; [
      # note: python lsps will use the current interpreter, and so will not see
      # vim extra python packages
      black
      flake8
      inotify-simple # ultisnips needs this
      mypy
      unidecode # ultisnips needs this
    ];

    extraLuaPackages = p: with p; [
      nvim-nio
    ];

    extraPackages = with pkgs; [
      delve # go debugger
      gofumpt # go fmt replacement
      gopls # go language server
      haskell-language-server
      haskellPackages.hlint
      lua-language-server
      nil # nix lsp
      nixpkgs-fmt
      ripgrep # for cocsearch
      tailwindcss-language-server
      tree-sitter
      watchman # for coc
      nodejs
    ];

    # The following symlinks vi, vim, vimdiff to the nvim equivalents.
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    coc.enable = true;
    # can override in local dir's .vim/coc-settings.json
    coc.settings = {
      "coc.source.around.enable" = false;
      "coc.preferences.jumpCommand" = "vsplit";
      "coc.preferences.formatOnSaveFiletypes" = [ "*" ];
      "diagnostic.displayByAle" = false;
      "diagnostic.refreshAfterSave" = true;
      "diagnostic.checkCurrentLine" = true;
      "codeLens" = {
        "enable" = true;
        "separator" = "▸";
        "subseparator" = "▹";
      };

      "inlayHint.enable" = false;

      "python.pythonPath" = "nvim-python3";
      "python.formatting.provider" = "black";
      "python.linting.flake8Enabled" = true;
      "python.linting.mypyEnabled" = true;

      "prettier.statusItemText" = "";

      "typescript" = {
        "format" = {
          "semicolons" = "insert";
          "insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces" = true;
        };
        "preferences" = {
          "importModuleSpecifier" = "project-relative";
          "quoteStyle" = "single";
        };
        "preferGoToSourceDefinition" = true;
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
        "lua" = {
          "command" = "${pkgs.lua-language-server}/bin/lua-language-server";
          "filetypes" = [ "lua" ];
          "settings" = {
            "Lua" = {
              "workspace" = {
                "library" = [
                  "${pkgs.neovim}/share/nvim/runtime/lua"
                  "${pkgs.vimPlugins.plenary-nvim}/lua"
                ];
              };
              "diagnostics" = {
                "enable" = true;
                "globals" = [ "vim" ];
              };
            };
          };
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

  # xdg.configFile.nvim = {
  #   source = ./configs;
  #   recursive = true;
  # };

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

