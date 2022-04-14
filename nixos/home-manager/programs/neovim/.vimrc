"""""""""""""""""""""""""""""FLAGS""""""""""""""""""""""""""
set autochdir
set autoindent
set autowriteall
set colorcolumn=80
set cursorline
set dictionary+=/usr/share/dict/words
set guicursor=
set hlsearch
set ignorecase
set incsearch
set lazyredraw
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
set shortmess+=ac
set showcmd
set signcolumn=yes
set smartcase
set smartindent
set smarttab
set splitbelow
set termguicolors
set title
set undofile
set whichwrap+=<,>,h,l,[,]
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

"""""" Latex
"Plug 'lervag/vimtex', {'for': 'tex'}
let g:tex_flavor = 'latex'
let g:vimtex_view_general_viewer='okular'
let g:vimtex_view_general_options='--unique --noraise file:@pdf\#src:@line@tex'
let g:vimtex_quickfix_method = 'pplatex'
let g:vimtex_compiler_latexmk = {
    \ 'build_dir' : "_latexmk",
    \ 'callback' : 1,
    \ 'continuous' : 1,
    \ 'executable' : 'latexmk',
    \ 'hooks' : [],
    \ 'options' : [
    \   '-verbose',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \   '-shell-escape',
    \ ],
\}

let g:vimtex_indent_ignored_envs = ['frame', 'document', 'center', 'figure',
    \ 'itemize', 'enumerate' ]
let g:vimtex_indent_on_ampersands = 0

nnoremap <leader>c :VimtexTocToggle<CR><c-w><c-h>

""""""Misc
"Plug 'scrooloose/nerdtree'

nnoremap <C-n> :NERDTreeMirror<CR>:NERDTreeToggle<CR>
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

" No line numbers in terminal (this breaks in vanilla vim)
autocmd TermOpen * setlocal nonumber norelativenumber

" File picker
let g:netrw_liststyle = 3
let g:netrw_banner = 0
let g:netrw_browse_split = 1
let g:netrw_winsize = 20

"""""""File Extension defaults
augroup TextProj
    au!
    autocmd BufRead,BufNewFile *.md,*.tex setlocal tw=80
    autocmd BufRead,BufNewFile *.md,*.tex setlocal spell spelllang=en_gb
augroup END


""""""""""""""""""""""""""GENERAL MAPS"""""""""""""""""""""

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
vnoremap <A-j> :m '>+1<CR>gv
vnoremap <A-k> :m '<-2<CR>gv

vnoremap <C-c> "+y
vnoremap <C-x> "+d

""""""""""""""""""""""""""""MISC""""""""""""""""""""""""""
