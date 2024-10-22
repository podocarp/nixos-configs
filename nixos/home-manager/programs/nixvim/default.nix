{ inputs, pkgs, ... }:
{
  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];
  programs.nixvim = {
    enable = true;
    # performance.byteCompileLua = {
    #   enable = true;
    #   configs = true;
    #   initLua = true;
    #   plugins = true;
    #   nvimRuntime = true;
    # };

    colorschemes.one.enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    opts = {
      autoindent = true;
      autowriteall = true;
      background = "light";
      colorcolumn = [ 80 ];
      complete = "";
      cursorline = true;
      expandtab = true;
      incsearch = true;
      ignorecase = true;
      number = true;
      relativenumber = true;
      ruler = true;
      scrolloff = 5;
      signcolumn = "yes";
      sessionoptions = "blank,buffers,curdir,folds,help,tabpages,winsize,winpos,terminal,localoptions";
      smartcase = true;
      smartindent = true;
      smarttab = true;
      splitbelow = true;
      title = true;
      undofile = true;
      updatetime = 300;
      whichwrap = "<,>,h,l,[,]";
      wildmenu = true;
      wildmode = [
        "longest"
        "list"
        "full"
      ];
    };

    autoCmd = [
      {
        callback.__raw = "function() vim.lsp.buf.format({ async = false }) end";
        event = [ "BufWritePre" ];
        pattern = [ "*" ];
      }
    ];

    keymaps =
      [
        {
          key = "<leader>c";
          mode = [ "n" ];
          action = ":VimtexTocToggle<CR>";
        }

        {
          key = "<C-n>";
          mode = [ "n" ];
          action = ":NvimTreeToggle<CR>";
        }
        {
          key = "<C-m>";
          mode = [ "n" ];
          action = ":NvimTreeFindFile<CR>";
        }

        {
          key = "<C-d>";
          mode = [
            "n"
            "i"
            "s"
          ];
          action.__raw = # lua
            ''
              function()
                if not require("noice.lsp").scroll(4) then
                  return "<c-d>"
                end
              end
            '';
          options = {
            expr = true;
          };
        }
        {
          key = "<C-u>";
          mode = [
            "n"
            "i"
            "s"
          ];
          action.__raw = # lua
            ''
              function()
                if not require("noice.lsp").scroll(-4) then
                  return "<c-u>"
                end
              end
            '';
          options = {
            expr = true;
          };
        }

        {
          key = "<C-j>";
          mode = [ "i" ];
          action.__raw = ''
            function() 
              if luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
              end
            end
          '';
        }
        {
          key = "<C-k>";
          mode = [ "i" ];
          action.__raw = ''
            function() 
              if luasnip.jumpable(-1) then
                luasnip.jump(-1)
              end
            end
          '';
        }

        {
          key = "gf";
          mode = [ "n" ];
          action.__raw = "function() require'fzf-lua'.lsp_finder() end";
        }
        {
          key = "gd";
          mode = [ "n" ];
          action.__raw = ''
            function()
              require'fzf-lua'.lsp_definitions({
                jump_to_single_result = true,
                jump_to_single_result_action = require('fzf-lua.actions').file_switch_or_edit,
              })
            end
          '';
        }
        {
          key = "gD";
          mode = [ "n" ];
          action.__raw = ''
            function()
              require'fzf-lua'.lsp_definitions({
                jump_to_single_result = true,
                jump_to_single_result_action = require('fzf-lua.actions').file_vsplit,
              })
            end
          '';
        }
        {
          key = "gy";
          mode = [ "n" ];
          action.__raw = ''
            function()
              require'fzf-lua'.lsp_typedefs({
                jump_to_single_result = true,
                jump_to_single_result_action = require('fzf-lua.actions').file_vsplit,
              })
            end
          '';
        }
        {
          key = "gca";
          mode = [ "n" ];
          action.__raw = "function() require'fzf-lua'.lsp_code_actions() end";
        }
        {
          key = "gi";
          mode = [ "n" ];
          action.__raw = "function() require'fzf-lua'.lsp_implementations({jump_to_single_result = true}) end";
        }
        {
          key = "gr";
          mode = [ "n" ];
          action.__raw = "function() require'fzf-lua'.lsp_references({jump_to_single_result = true}) end";
        }
        {
          key = "<leader>a";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.diagnostics_workspace";
        }
        {
          key = "<leader>g";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.git_files";
        }
        {
          key = "<leader>s";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.lsp_live_workspace_symbols";
        }
        {
          key = "<leader>f";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.grep_project";
        }
        {
          key = "<leader>c";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.builtin";
        }

        {
          key = "<C-c>";
          mode = [ "v" ];
          action = "\"+y";
        }
        {
          key = "<C-x>";
          mode = [ "v" ];
          action = "\"+d";
        }
        {
          key = "<C-v>";
          mode = [ "i" ];
          action = "<C-o>\"+p";
        }
        {
          key = "<leader>d";
          mode = [ "n" ];
          action = "\"_d";
        }

        {
          key = "<C-j>";
          mode = [ "v" ];
          action = ":m '>+1<CR>gv";
        }
        {
          key = "<C-k>";
          mode = [ "v" ];
          action = ":m '<-2<CR>gv";
        }

        {
          key = "<F3>";
          mode = [ "n" ];
          action = ":noh<CR>";
        }
        {
          key = "<Esc>";
          mode = [ "n" ];
          action = ":update!<CR>";
          options.silent = true;
        }
      ]
      ++ (builtins.map
        (key: {
          key = "<C-${key}>";
          mode = [ "n" ];
          action = "<C-W><C-${key}>";
          options.desc = "Move focus between split windows ${key}";
        })
        [
          "h"
          "j"
          "k"
          "l"
        ]
      )
      ++ (builtins.map
        (key: {
          key = "<C-w>${key}";
          mode = [ "n" ];
          action = "<C-W><S-${key}>";
          options.desc = "Move split windows ${key}";
        })
        [
          "h"
          "j"
          "k"
          "l"
        ]
      );

    extraPackages = with pkgs; [
      nixfmt-rfc-style
      tailwindcss-language-server
    ];

    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin rec {
        pname = "tailwind-tools";
        version = "v0.3.1";
        src = pkgs.fetchFromGitHub {
          owner = "luckasRanarison";
          repo = "tailwind-tools.nvim";
          rev = version;
          sha256 = "+mtymFxOmTMvkIFvoeHObyM9+yPhQ+z3Bo6t1HQPY6A=";
        };
        buildInputs = [ ];
      })
    ];

    highlight = {
      PmenuSel = {
        fg = "white";
        bold = true;
      };
      IncSearch = {
        link = "Search";
        force = true;
      };
    };

    extraConfigLuaPre = # lua
      ''
        local luasnip = require "luasnip"
        require("tailwind-tools").setup({ })
      '';

    extraConfigLuaPost = # lua
      ''
        vim.api.nvim_set_hl(0, "IncSearch", {
              force = true,
              link = "Search",
        })
        vim.api.nvim_set_hl(0, "PmenuSel", {
              bold = true,
              fg = "white",
        })
      '';

    plugins = {
      auto-session.enable = true;

      cmp = {
        enable = true;
        autoEnableSources = true;
        settings = {
          sources = [
            { name = "path"; }
            { name = "nvim_lsp"; }
            {
              name = "buffer";
              keyword_length = 3;
            }
            { name = "luasnip"; }
            { name = "nvim_lsp_signature_help"; }
            { name = "cmdline"; }
          ];
          snippet = {
            expand = "function(args) luasnip.lsp_expand(args.body) end";
          };
          formatting.format = ''
            function(entry, item)
              local menu_icon = {
                nvim_lsp = 'λ',
                luasnip = 'S',
                buffer = 'B',
                path = 'P',
              }

              item.menu = menu_icon[entry.source.name]
              return item
            end
          '';
          view.entries = {
            name = "custom";
            selection_order = "near_cursor";
          };
          mapping = {
            "<C-u>" = "cmp.mapping.scroll_docs(-4)";
            "<C-d>" = "cmp.mapping.scroll_docs(4)";
            # C-b (back) C-f (forward) for snippet placeholder navigation.
            "<C-y>" = "cmp.mapping.confirm {select = true}";
            "<C-Y>" = "cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true }";
            "<C-n>" = # lua
              ''
                cmp.mapping(function(fallback)
                  if cmp.visible() then
                    cmp.select_next_item()
                  else
                    fallback()
                  end
                end, { 'i', 's' })
              '';
            "<C-p>" = # lua
              ''
                cmp.mapping(function(fallback)
                  if cmp.visible() then
                    cmp.select_prev_item()
                  else
                    fallback()
                  end
                end, { 'i', 's' })
              '';
          };
        };
      };

      dap = {
        enable = true;
        adapters.executables = {
          python = {
            command = "python";
            args = [
              "-m"
              "debugpy.adapter"
            ];
          };
        };
        configurations = { };
        signs = {
          dapBreakpoint.text = "";
          dapBreakpointRejected.text = "";
          dapBreakpointCondition.text = "";
          dapLogPoint.text = "";
          dapStopped.text = "→";
        };
        extensions = {
          dap-go = {
            enable = true;
          };
          dap-ui = {
            enable = true;
            layouts = [
              {
                elements = [
                  {
                    id = "breakpoints";
                    size = 0.1;
                  }
                  {
                    id = "scopes";
                    size = 0.6;
                  }
                  {
                    id = "stacks";
                    size = 0.15;
                  }
                  {
                    id = "watches";
                    size = 0.15;
                  }
                ];
                position = "left";
                size = 40;
              }
              {
                elements = [
                  {
                    id = "repl";
                    size = 0.5;
                  }
                  {
                    id = "console";
                    size = 0.5;
                  }
                ];
                position = "bottom";
                size = 10;
              }
            ];
          };
        };
      };

      friendly-snippets.enable = true;

      gitsigns.enable = true;

      luasnip = {
        enable = true;
      };

      lsp = {
        enable = true;
        keymaps = {
          diagnostic = {
            "]g" = "goto_next";
            "[g" = "goto_prev";
            "<C-w>d" = "open_float";
          };
          lspBuf = {
            "<F2>" = "rename";
            "K" = "hover";
          };
          extra = [
            {
              action = "";
              key = "<leader>lx";
            }
          ];
        };
        servers = {
          gopls = {
            enable = true;
            settings = {
              gopls = {
                experimentalPostfixCompletions = true;
                analyses = {
                  unusedparams = true;
                  shadow = true;
                };
                staticcheck = true;
              };
            };
            onAttach.function = ''
              vim.api.nvim_create_autocmd("BufWritePre", {
                buffer = bufnr,
                callback = function()
                  vim.lsp.buf.code_action { context = { only = { 'source.organizeImports' } }, apply = true }
                  vim.lsp.buf.code_action { context = { only = { 'source.fixAll' } }, apply = true }
                end
              })
            '';
          };
          nixd = {
            enable = true;
            onAttach.function = "client.server_capabilities.semanticTokensProvider = nil";
            settings = {
              formatting.command = [ "nixfmt" ];
            };
          };
          lua_ls = {
            enable = true;
            onAttach.function = ''
              if client.workspace_folders then
                local path = client.workspace_folders[1].name
                if vim.uv.fs_stat(path..'/.luarc.json') or vim.uv.fs_stat(path..'/.luarc.jsonc') then
                  return
                end
              end
            '';
            settings = {
              workspace.library = [ { __raw = "vim.env.VIMRUNTIME"; } ];
              workspace.ignoreDir = [
                ".direnv"
                "node_modules"
                ".git"
                "dist"
                "build"
              ];
            };
          };

          eslint = {
            enable = true;
            onAttach.function = ''
              vim.api.nvim_create_autocmd("BufWritePre", {
                buffer = bufnr,
                command = "EslintFixAll",
              })
            '';
          };
          ts_ls.enable = true;
        };
      };

      lsp-signature = {
        enable = false;
        settings = {
          handler_opts = {
            border = "none";
          };
          hint_prefix = "";
        };
      };

      lualine = {
        enable = true;
        settings = {
          extensions = [ "nvim-tree" ];
          options = {
            icons_enabled = true;
            theme = "onelight";
          };
          sections = {
            lualine_a = [ "mode" ];
            lualine_b = [
              "branch"
              "diff"
            ];
            lualine_c = [
              "diagnostics"
            ];
            lualine_x = [
              {
                __unkeyed-1 = "filename";
                path = 1;
              }
            ];
            lualine_y = [
              "progress"
              "location"
            ];
            lualine_z = [
              ""
            ];
          };
        };
      };

      noice = {
        enable = true;
        lsp.override = {
          "cmp.entry.get_documentation" = true;
          "vim.lsp.util.convert_input_to_markdown_lines" = true;
          "vim.lsp.util.stylize_markdown" = true;
        };
        routes = [
          {
            filter = {
              event = "msg_show";
              kind = "";
              rind = "written";
            };
            opts = {
              skip = true;
            };
          }
        ];
        views = {
          mini = {
            timeout = 5000;
            focusable = true;
            border = {
              style = "rounded";
            };
            win_options = {
              winhighlight = {
                Normal = "NormalFloat";
              };
            };
          };
        };
      };

      nvim-surround.enable = true;

      nvim-tree = {
        enable = true;
        autoClose = true;
        disableNetrw = true;
        git = {
          enable = true;
          ignore = true;
        };
        view = {
          signcolumn = "no";
          centralizeSelection = true;
          float = {
            enable = true;
            openWinConfig.__raw = # lua
              ''
                function()
                  local HEIGHT_RATIO = 0.8 -- You can change this
                  local WIDTH_RATIO = 0.5  -- You can change this too
                  local screen_w = vim.opt.columns:get()
                  local screen_h = vim.opt.lines:get() - vim.opt.cmdheight:get()
                  local window_w = screen_w * WIDTH_RATIO
                  local window_h = screen_h * HEIGHT_RATIO
                  local window_w_int = math.floor(window_w)
                  local window_h_int = math.floor(window_h)
                  local center_x = (screen_w - window_w) / 2
                  local center_y = ((vim.opt.lines:get() - window_h) / 2)
                      - vim.opt.cmdheight:get()
                  return {
                    border = 'rounded',
                    relative = 'editor',
                    row = center_y,
                    col = center_x,
                    width = window_w_int,
                    height = window_h_int,
                  }
                end
              '';
          };
        };
        onAttach.__raw = # lua
          ''
            function(bufnr)
              local api = require('nvim-tree.api')

              local function opts(desc)
                return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
              end

              local function edit_or_open()
                local node = api.tree.get_node_under_cursor()

                if node.nodes ~= nil then
                  -- expand or collapse folder
                  api.node.open.edit()
                else
                  -- open file
                  api.node.open.edit()
                  -- Close the tree if file was opened
                  api.tree.close()
                end
              end

              -- open as vsplit on current node
              local function vsplit_preview()
                local node = api.tree.get_node_under_cursor()

                if node.nodes ~= nil then
                  -- expand or collapse folder
                  api.node.open.edit()
                else
                  -- open file as vsplit
                  api.node.open.vertical()
                end

                -- Finally refocus on tree if it was lost
                api.tree.focus()
              end

              api.config.mappings.default_on_attach(bufnr)

              vim.keymap.set('n', 'u', api.tree.change_root_to_parent, opts('Up'))
              vim.keymap.set("n", "l", edit_or_open, opts("Edit Or Open"))
              vim.keymap.set("n", "L", vsplit_preview, opts("Vsplit Preview"))
              vim.keymap.set("n", "h", api.node.navigate.parent_close, opts("Close"))
              vim.keymap.set("n", "H", api.tree.collapse_all, opts("Collapse All"))
            end
          '';
      };

      fugitive.enable = true;

      fzf-lua = {
        enable = true;
        profile = "default";
      };

      sleuth.enable = true;

      treesitter = {
        enable = true;
        settings = {
          auto_install = false;
          highlight.enable = true;
          incremental_selection = {
            enable = true;
            keymaps = {
              init_selection = false;
              node_decremental = "ghm";
              node_incremental = "ghn";
              scope_incremental = "ghc";
            };
          };
          indent = {
            enable = true;
          };
        };
      };

      treesitter-context = {
        enable = true;
        settings = {
          line_numbers = true;
          max_lines = 3;
          min_window_height = 10;
          multiline_threshold = 20;
          mode = "cursor";
          trim_scope = "outer";
        };
      };

      ts-autotag.enable = true;

      web-devicons.enable = true;
    };
  };
}
