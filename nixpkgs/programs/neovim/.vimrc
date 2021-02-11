"""""""""""""""""""""""""""""FLAGS""""""""""""""""""""""""""
" set statusline+=%F\ %l:%c
set autochdir
set autoindent
set autowriteall
set colorcolumn=80
set cursorline
set dictionary+=/usr/share/dict/words
set expandtab
set ignorecase
set incsearch
set lazyredraw
set linebreak
set list
set mouse=a
set nobackup
set nojoinspaces
set noshowmode
set nospell
set nowrapscan
set number
set path+=**
set relativenumber
set regexpengine=1
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

let g:ale_disable_lsp = 1
"Plug 'w0rp/ale'
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_linters = {
    \'c': ['clang'],
    \'cpp': ['clang'],
    \'python': ['pylint'],
    \'tex': ['chktex'],
    \'haskell': ['hlint']
            \}
let g:ale_fixers = {
    \'*': ['remove_trailing_lines', 'trim_whitespace'],
    \'c': ['clang-format', 'trim_whitespace'],
    \'cpp': ['clang-format', 'trim_whitespace'],
    \'java': ['prettier', 'trim_whitespace'],
    \'python': ['black', 'trim_whitespace'],
    \'haskell': ['hlint', 'trim_whitespace']
            \}
let g:ale_c_clang_options='-std=c11 -Wall -pthread'
let g:ale_c_gcc_options='-std=c11 -Wall -lpthread'
let g:ale_echo_msg_error_str='E'
let g:ale_echo_msg_warning_str='W'
let g:ale_echo_msg_info_str='I'
let g:ale_echo_msg_format='[%linter%][%severity%] %s'
let g:ale_max_signs=20

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
let g:vimtex_view_method='zathura'
let g:vimtex_view_zathura_check_libsynctex=0
" let g:vimtex_view_general_viewer='okular'
" let g:vimtex_view_general_options='--unique file:@pdf\#src:@line@tex'
" let g:vimtex_view_general_options_latexmk=''
let g:vimtex_syntax_alpha=1
let g:vimtex_compiler_latexmk = {
            \ 'backend' : 'nvim',
            \ 'background' : 1,
            \ 'build_dir' : "_latexmk",
            \ 'callback' : 1,
            \ 'continuous' : 1,
            \ 'executable' : 'latexmk',
            \ 'hooks' : [],
            \ 'options' : [
            \   '-verbose',
            \   '-file-line-error',
            \   '-interaction=nonstopmode',
            \   '-synctex=1',
            \   '-shell-escape',
            \   '-pdf',
            \ ],
            \}

nnoremap <leader>c :VimtexTocToggle<CR><c-w><c-h>

""""""CSV
"Plug 'chrisbra/csv.vim', {'for': 'csv'}
let b:csv_arrange_align = 'l*'

""""""Haskell
"Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}

""""""Misc
"Plug 'scrooloose/nerdtree'
autocmd StdinReadPre * let s:std_in=1
" Open NERDTree if no file specified
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" Open NERDTree if opening a directory
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | exe 'cd '.argv()[0] | endif
" Close vim if the only window left is NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1
let g:NERDTreeDirArrowExpandable = '+'
let g:NERDTreeDirArrowCollapsible = '-'
let g:NERDTreeNodeDelimiter = "\u00a0"

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
let g:gruvbox_italic=1
let g:gruvbox_italicize_comments=0
let g:gruvbox_italicize_strings=1
let g:gruvbox_contrast_dark='hard'
let g:gruvbox_contrast_light='soft'
let g:gruvbox_invert_indent_guides=1
let g:gruvbox_sign_column='bg0'
""""""
"call plug#end()
"""""""""""""""""""""""""""AESTHETICS""""""""""""""""""""""
" Colorscheme
colorscheme gruvbox
" hi Comment guifg=#999090
hi CocCodeLens guibg=#333333 guifg=#999090
hi Normal guibg=#000000
hi LineNr guibg=#202020
hi SignColumn guibg=#202020
hi ALEErrorSign guibg=#202020
hi ALEWarningSign guibg=#202020

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
    autocmd BufRead,BufNewFile *.md set tw=80
    autocmd BufRead,BufNewFile *.md set spell spelllang=en_gb
    autocmd BufRead,BufNewFile *.md setlocal syntax=off
augroup END

augroup TexProj
    au!
    autocmd BufRead,BufNewFile *.tex set tw=80
    autocmd BufRead,BufNewFile *.tex set spell spelllang=en_gb
augroup END

augroup CProj
    au!
    autocmd BufRead,BufNewFile *.c,*.cpp,*.h set cindent shiftwidth=8
augroup END

augroup JSProj
    au!
    autocmd BufRead,BufNewFile *.js,*.jsx,*.ts,*.tsx set filetype=javascript
    autocmd BufRead,BufNewFile *.js,*.jsx,*.ts,*.tsx set softtabstop=2 shiftwidth=2
augroup END

augroup PyProj
    au!
    autocmd BufRead,BufNewFile *.py set softtabstop=4 shiftwidth=4
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
