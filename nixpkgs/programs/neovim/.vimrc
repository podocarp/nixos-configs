"""""""""""""""""""""""""""""FLAGS""""""""""""""""""""""""""
" set statusline+=%F\ %l:%c
set autochdir
set autoindent
set autowriteall
set colorcolumn=80
set cursorline
set dictionary+=/usr/share/dict/words
set expandtab
set incsearch
set lazyredraw
set linebreak
set list
set mouse=a
set nobackup
set nojoinspaces
set nowrapscan
set number
set path+=**
set relativenumber
set ruler
set scrolloff=4
set shada=:10,'10,%,n~/.local/share/nvim/shada/main.shada
set shiftwidth=4
set shortmess+=ac
set showcmd
set signcolumn=yes
set smartcase
set smartindent
set smarttab
set splitright
set termguicolors
set title
set undofile
set wildmenu
set wildmode=longest,list,full

"""""""""""""""""""""""""""PLUGINS"""""""""""""""""""""""""""""
"call plug#begin(stdpath('data') . '/bundle')

""""""" Autocomplete and syntax
"Plug 'SirVer/ultisnips'
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

"Plug 'honza/vim-snippets'

"Plug 'neoclide/coc.nvim', {'branch': 'release'}
nnoremap <silent> <leader>h :call CocActionAsync('doHover')<cr>
nmap <silent> co <Plug>(coc-codelens-action)

"Plug 'w0rp/ale'
" let g:ale_disable_lsp = 1
" let g:ale_lint_on_text_changed = 'never'
" let g:ale_lint_on_insert_leave = 0
" let g:ale_linters = {
"     \'c': ['clang'],
"     \'cpp': ['clang'],
"     \'haskell': ['hlint'],
"     \'javascript': ['eslint'],
"     \'python': ['pylint'],
"     \'typescript': ['eslint', 'tsserver'],
"     \'tex': ['chktex']
"     \}
" let g:ale_fixers = {
"     \'*': ['remove_trailing_lines', 'trim_whitespace'],
"     \'c': ['clang-format'],
"     \'cpp': ['clang-format'],
"     \'haskell': ['hlint'],
"     \'java': ['prettier'],
"     \'javascript': ['eslint'],
"     \'python': ['black'],
"     \'typescript': ['prettier']
"     \}
" let g:ale_c_clang_options='-std=c11 -Wall -pthread'
" let g:ale_c_gcc_options='-std=c11 -Wall -lpthread'
" let g:ale_echo_msg_error_str='E'
" let g:ale_echo_msg_warning_str='W'
" let g:ale_echo_msg_info_str='I'
" let g:ale_echo_msg_format='[%linter%][%severity%] %s'
" let g:ale_max_signs=20

nmap <silent> zk <Plug>(ale_previous_wrap)zz
nmap <silent> zj <Plug>(ale_next_wrap)zz
nmap <silent> <leader>* <Plug>(ale_find_references)
nmap <silent> <leader>? <Plug>(ale_go_to_definition)
nmap <silent> <leader>/ <Plug>(ale_detail)
noremap <F1> :ALEFix<CR>

"""""" Latex
"Plug 'lervag/vimtex', {'for': 'tex'}
let g:tex_flavor = 'latex'
let g:vimtex_compiler_progname='nvr'
" let g:vimtex_view_method='zathura'
" let g:vimtex_view_zathura_check_libsynctex=0
let g:vimtex_view_general_viewer='okular'
let g:vimtex_view_general_options='--unique file:@pdf\#src:@line@tex'
let g:vimtex_view_general_options_latexmk = '--unique'
let g:vimtex_syntax_conceal = {
    \ 'math_bounds' : 0,
    \ 'math_fracs' : 0,
\}
set conceallevel=2
let g:tex_conceal="abdgm"

let g:vimtex_compiler_latexmk = {
    \ 'build_dir' : "_latexmk",
    \ 'callback' : 1,
    \ 'continuous' : 1,
    \ 'executable' : 'latexmk',
    \ 'hooks' : [],
    \ 'options' : [
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \   '-shell-escape',
    \ ],
\}

let g:vimtex_indent_ignored_envs = ['frame', 'document', 'center', 'figure']
let g:vimtex_indent_tikz_enabled = 0

nnoremap <leader>c :VimtexTocToggle<CR><c-w><c-h>

""""""Haskell
"Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}

""""""Misc
"Plug 'scrooloose/nerdtree'

" Start NERDTree, unless a file or session is specified, eg. vim -S session_file.vim.
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists('s:std_in') && v:this_session == '' | NERDTree | endif
" Close the tab if NERDTree is the only window remaining in it.
autocmd BufEnter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * if getcmdwintype() == '' | silent NERDTreeMirror | endif

map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '-'
let g:NERDTreeNodeDelimiter = "\u00a0"

"Plug 'mbbill/undotree'
nnoremap <F1> :UndotreeToggle<CR>

"Plug 'wincent/command-t', {'do': 'cd ruby/command-t/ext/command-t && ruby extconf.rb && make'}
let g:CommandTCursorColor = 'Search'
let g:CommandTHighlightColor = 'Search'

"Plug 'vim-airline/vim-airline'
let g:airline_symbols_ascii=1
let g:airline_section_x=''
let g:airline_section_y=''
let g:airline_section_z='%l:%c %p%%'
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#whitespace#enabled = 0
"Plug 'tpope/vim-surround'

"Plug 'morhetz/gruvbox'
" let g:gruvbox_contrast_dark='hard'
" let g:gruvbox_contrast_light='hard'
" let g:gruvbox_invert_indent_guides=1
" let g:gruvbox_sign_column='bg0'
" let g:gruvbox_colors = { 'bg0': ['#000000', 0] }

""""""
"call plug#end()
"""""""""""""""""""""""""""AESTHETICS""""""""""""""""""""""
" Colorscheme
"Plug 'NLKNguyen/papercolor-theme'
set background=light
let g:airline_theme='papercolor'
let g:PaperColor_Theme_Options = {
    \ 'theme': {
    \   'default.light': {
    \     'override' : {
    \       'color00' : ['#ffffff', '255'],
    \       'linenumber_fg' : ['#000000', '255'],
    \       'linenumber_bg' : ['#ffffff', '255'],
    \       'visual_fg' : ['#ffffff', '255'],
    \     }
    \   }
    \ }
\ }
colorscheme PaperColor
" hi CocCodeLens guibg=#333333 guifg=#999090
" hi ALEErrorSign guibg=#202020
" hi ALEWarningSign guibg=#202020

" Disable neovim insert mode bar cursor
set guicursor=

" Highlight search
set hlsearch

" Scrolling past the line pops you below
set whichwrap+=<,>,h,l,[,]

" No line numbers in terminal (this breaks in vanilla vim)
autocmd TermOpen * setlocal nonumber norelativenumber

" File picker
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_browse_split = 1
let g:netrw_winsize = 20

"""""""File Extension defaults
augroup MDProj
    au!
    autocmd BufRead,BufNewFile *.md setlocal tw=80
    autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_gb
    autocmd BufRead,BufNewFile *.md setlocal syntax=off
augroup END

augroup TexProj
    au!
    autocmd BufRead,BufNewFile *.tex setlocal tw=80
    autocmd BufRead,BufNewFile *.tex setlocal spell spelllang=en_gb
    autocmd BufRead,BufNewFile *.tex setlocal indentkeys-=o,O
augroup END

augroup CProj
    au!
    autocmd BufRead,BufNewFile *.c,*.cpp,*.h setlocal cindent shiftwidth=8
augroup END

augroup JSProj
    au!
    autocmd BufRead,BufNewFile *.js,*.jsx,*.ts,*.tsx setlocal softtabstop=2 shiftwidth=2
augroup END

augroup PyProj
    au!
    autocmd BufRead,BufNewFile *.py setlocal softtabstop=4 shiftwidth=4
augroup END

""""""""""""""""""""""""""GENERAL MAPS"""""""""""""""""""""
" Scroll in wrapped lines
map <Up> gk
map <Down> gj
""""""""""""""""""""""""INSERT MODE MAPS"""""""""""""""""""

" paste
inoremap <C-v> <ESC>"+pa
" dictionary completion
inoremap <C-k> <C-x><C-x><C-k>

""""""""""""""""""""""""NORMAL MODE MAPS"""""""""""""""""""

" Sends things away without overwriting the register
nnoremap <leader>d "_d

" Open shell (this breaks in vanilla vim)
nnoremap <C-S> :sp<CR>:term<CR>A

" Remove search highlights
nnoremap <F3> :noh<CR>

nnoremap <Esc> :w<CR>

""""""""""""""""""""""SPLIT WINDOWS""""""""""""""""""""""""

" For moving split windows
nnoremap <C-W>h <C-W><S-H>
nnoremap <C-W>j <C-W><S-J>
nnoremap <C-W>k <C-W><S-K>
nnoremap <C-W>l <C-W><S-L>
" For moving focus between split windows
nnoremap <C-h> <C-W><C-h>
nnoremap <C-j> <C-W><C-j>
nnoremap <C-k> <C-W><C-k>
nnoremap <C-l> <C-W><C-l>

"""""""""""""""""""""VISUAL MODE MAPS""""""""""""""""""""""
" Sends things away without overwriting the register
vnoremap <leader>d "_d

" Move selected blocks up and down
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv

vnoremap <C-c> "+y
vnoremap <C-x> "+d

""""""""""""""""""""""""""""MISC""""""""""""""""""""""""""
