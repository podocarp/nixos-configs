{ inputs, pkgs, ... }:
{
  imports = [
    inputs.nixvim.homeManagerModules.nixvim
  ];
  programs.nixvim = {
    enable = true;
    performance.byteCompileLua = {
      enable = true;
      configs = true;
      initLua = true;
      plugins = true;
      nvimRuntime = true;
    };

    colorschemes.one.enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    opts = {
      autoindent = true;
      autowriteall = true;
      background = "light";
      backupdir = "/tmp/nvim/backups";
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
      wrapscan = false;
    };

    autoCmd = [ ];

    keymaps =
      [
        {
          key = "<leader>gd";
          mode = [ "n" ];
          action = ":Gdiff<CR>";
          options = {
            desc = "Git diff";
          };
        }

        {
          key = "<C-n>";
          mode = [ "n" ];
          action = ":NvimTreeToggle<CR>";
          options.silent = true;
        }
        {
          key = "<leader>n";
          mode = [ "n" ];
          action = ":NvimTreeFindFile<CR>";
          options.silent = true;
        }

        {
          key = "<C-g>n";
          mode = [ "n" ];
          action = ":NvimTreeFindFile<CR>";
          options.silent = true;
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
          options.desc = "Go to definition";
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
          options.desc = "View definition in a split";
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
          options.desc = "View type definition in a split";
        }
        {
          key = "gca";
          mode = [ "n" ];
          action.__raw = "function() require'fzf-lua'.lsp_code_actions() end";
          options.desc = "View lsp code actions";
        }
        {
          key = "gi";
          mode = [ "n" ];
          action.__raw = "function() require'fzf-lua'.lsp_implementations({jump_to_single_result = true}) end";
          options.desc = "View lsp implementations";
        }
        {
          key = "gr";
          mode = [ "n" ];
          action.__raw = "function() require'fzf-lua'.lsp_references({jump_to_single_result = true}) end";
          options.desc = "View lsp references";
        }
        {
          key = "<leader>a";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.diagnostics_workspace";
          options.desc = "Workspace diagnostics";
        }
        {
          key = "<leader>t";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.git_files";
          options.desc = "Search through files tracked by git";
        }
        {
          key = "<leader>s";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.lsp_live_workspace_symbols";
          options.desc = "Search through workspace symbols";
        }
        {
          key = "<leader>f";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.grep_project";
          options.desc = "Grep through project";
        }
        {
          key = "<leader>c";
          mode = [ "n" ];
          action.__raw = "require'fzf-lua'.builtin";
          options.desc = "Other fzf-lua commands";
        }

        {
          key = "<C-c>";
          mode = [ "v" ];
          action = "\"+y";
          options.desc = "Copy to system clipboard";
        }
        {
          key = "<C-x>";
          mode = [ "v" ];
          action = "\"+d";
          options.desc = "Cut to system clipboard";
        }
        {
          key = "<C-v>";
          mode = [ "i" ];
          action = "<C-o>\"+p";
          options.desc = "Paste from system clipboard";
        }
        {
          key = "<leader>d";
          mode = [ "n" ];
          action = "\"_d";
          options.desc = "Delete into black hole register";
        }

        {
          key = "<C-j>";
          mode = [ "v" ];
          action = ":m '>+1<CR>gv";
          options.desc = "Shift selected line down";
        }
        {
          key = "<C-k>";
          mode = [ "v" ];
          action = ":m '<-2<CR>gv";
          options.desc = "Shift selected line up";
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

    extraConfigLuaPre = # lua
      ''
        require("tailwind-tools").setup({ })
        local noice = require "noice"

        local luasnip = require "luasnip"

        local list_snips = function()
          local ft_list = luasnip.available()[vim.o.filetype]
          local ft_snips = {}
          for _, item in pairs(ft_list) do
            ft_snips[item.trigger] = item.name
          end
          print(vim.inspect(ft_snips))
        end

        vim.api.nvim_create_user_command("SnipList", list_snips, {})
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
              bg = "black",
        })
      '';

    plugins = {
      auto-session.enable = true;

      cmp = {
        enable = true;
        autoEnableSources = true;
        settings = {
          sources = [
            { name = "nvim_lsp"; }
            { name = "luasnip"; }
            { name = "path"; }
            {
              name = "buffer";
              keyword_length = 3;
            }
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

      gitsigns.enable = true;

      luasnip = {
        enable = true;
        fromLua = [
          {
            paths = ./snippets;
            lazyLoad = true;
          }
        ];
        filetypeExtend = {
          typescriptreact = [ "typescript" ];
        };
      };

      lsp = {
        enable = true;
        keymaps = {
          diagnostic = {
            "]g" = "goto_next";
            "[g" = "goto_prev";
            "gh" = "open_float";
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
        onAttach = # lua
          ''
            vim.api.nvim_create_autocmd("BufWritePre", {
              buffer = bufnr,
              callback = function() vim.lsp.buf.format({ async = false }) end
            })
            vim.api.nvim_create_autocmd("CursorHold", {
              buffer = bufnr,
              callback = vim.lsp.buf.document_highlight,
            })
            vim.api.nvim_create_autocmd("CursorMoved", {
              buffer = bufnr,
              callback = vim.lsp.buf.clear_references,
            })
          '';
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
            onAttach.function = # lua
              ''
                vim.api.nvim_create_autocmd("BufWritePre", {
                  buffer = bufnr,
                  callback = function()
                    local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", {only = {"source.organizeImports"}})
                    for cid, res in pairs(result or {}) do
                      for _, r in pairs(res.result or {}) do
                        if r.edit then
                          local enc = (vim.lsp.get_client_by_id(cid) or {}).offset_encoding or "utf-16"
                          vim.lsp.util.apply_workspace_edit(r.edit, enc)
                        end
                      end
                    end
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
              workspace = {
                library = [
                  { __raw = "vim.env.VIMRUNTIME"; }
                  { __raw = "vim.api.nvim_get_runtime_file('*lua/luasnip', false)[1]"; }
                ];
                checkThirdParty = false;
              };
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
              {
                __unkeyed-1 = "filename";
                path = 1;
              }
            ];
            lualine_x = [
              {
                __unkeyed-1.__raw = "noice.api.statusline.mode.get";
                cond.__raw = "noice.api.statusline.mode.has";
                color = {
                  fg = "#ff9e64";
                };
              }
            ];
            lualine_y = [
              "progress"
              "location"
            ];
            lualine_z = [
              {
                __unkeyed-1 = "diagnostics";
                sources = [
                  "nvim_lsp"
                  "nvim_diagnostic"
                ];
                sections = [
                  "error"
                  "warn"
                  "info"
                  "hint"
                ];
              }
            ];
          };
        };
      };

      neogen = {
        enable = true;
        snippetEngine = "luasnip";
        keymaps.generate = "<leader>gc";
        inputAfterComment = true;
        languages = rec {
          typescript = {
            template = {
              annotation_convention = "tsdoc";
            };
          };
          typescriptreact = typescript;
        };
      };

      noice = {
        enable = true;
        notify.view = "mini";
        lsp.override = {
          "cmp.entry.get_documentation" = true;
          "vim.lsp.util.convert_input_to_markdown_lines" = true;
          "vim.lsp.util.stylize_markdown" = true;
        };
        presets = {
          long_message_to_split = true;
        };
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

      refactoring = {
        enable = true;
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
              init_selection = "<CR>";
              node_incremental = "<CR>";
              scope_incremental = "<S-CR>";
              node_decremental = "<BS>";
            };
          };
          indent.enable = true;
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

      which-key = {
        enable = true;
      };
    };
  };
}
