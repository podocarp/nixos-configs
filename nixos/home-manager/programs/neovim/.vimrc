"""""""""""""""""""""""""""""FLAGS""""""""""""""""""""""""""
set autoindent
set autowriteall
set colorcolumn=80
set cursorline
set dictionary+=/usr/share/dict/words
set expandtab
set guicursor=
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
set signcolumn=yes
set smartcase
set smartindent
set smarttab
set splitbelow
set termguicolors
set title
set undofile
set updatetime=300
set whichwrap+=<,>,h,l,[,]
set wildmenu
set wildmode=longest,list,full
let g:vimsyn_embed='l'

"""""""""""""""""""""""""""PLUGINS"""""""""""""""""""""""""""""
"call plug#begin(stdpath('data') . '/bundle')

""""""" Autocomplete and syntax
"Plug 'SirVer/ultisnips'
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

"Plug 'honza/vim-snippets'

"Plug 'mfussenegger/nvim-dap'
"Plug 'rcarriga/nvim-dap-ui'

"Plug 'neoclide/coc.nvim', {'branch': 'release'}
set tagfunc=CocTagFunc
inoremap <silent><expr> <c-c> coc#refresh()
nmap <silent> gd :call CocAction('jumpDefinition', 'tab drop')<CR>
nmap <silent> gy :call CocAction('jumpTypeDefinition', 'vsplit')<CR>
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr :call CocAction('jumpUsed')<CR>
nmap <silent> gh :call ShowDocumentation()<CR>
function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('definitionHover')
  endif
endfunction
autocmd CursorHold * silent call CocActionAsync('highlight')
" Symbol renaming.
nmap <F2> <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>=  <Plug>(coc-format-selected)
nmap <leader>=  <Plug>(coc-format-selected)
augroup cocgroup
  autocmd!
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Run the Code Lens action on the current line.
nmap <leader>cl  <Plug>(coc-codelens-action)

" Jump to next diagnostic (errors, etc)
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

imap <C-j> <Plug>(coc-snippets-expand-jump)

" Remap <C-f> and <C-b> for scroll float windows/popups.
nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>

" Auto import on go files
autocmd BufWritePre *.go :silent call CocAction('runCommand', 'editor.action.organizeImport')

" keyword for scss
autocmd FileType scss setl iskeyword+=@-@

"""""" Latex
"Plug 'lervag/vimtex', {'for': 'tex'}
let g:vimtex_view_method = 'zathura'
" let g:vimtex_view_general_options = '--noraise --unique file:@pdf\#src:@line@tex'
let g:vimtex_compiler_latexmk = {
    \ 'build_dir' : "_latexmk",
    \ 'callback' : 1,
    \ 'continuous' : 1,
    \ 'executable' : 'latexmk',
    \ 'hooks' : [],
    \ 'options' : [
    \   '-file-line-error',
    \   '-interaction=nonstopmode',
    \   '-shell-escape',
    \   '-synctex=1',
    \   '-verbose',
    \ ],
\}
let g:vimtex_indent_ignored_envs = ['frame', 'document', 'center', 'figure',
    \ 'itemize', 'enumerate' ]
let g:vimtex_indent_on_ampersands = 0

nnoremap <leader>c :VimtexTocToggle<CR><c-w><c-h>


""""""Misc

" Disable netrw
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1
nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <leader>f :NvimTreeFindFile<CR>

"Plug 'mbbill/undotree'
nnoremap <F1> :UndotreeToggle<CR>
let g:undotree_SetFocusWhenToggle = 1

"Plug 'junegunn/fzf.vim'
nnoremap <leader>t :GFiles --cached --others --exclude-standard<CR>
nnoremap <leader>s :Rg<CR>

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
    \     'transparent_background': 0,
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
highlight CocMenuSel guibg=#13354A guifg=#ffffff gui=bold
highlight link NvimTreeNormalFloat Normal

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


""""""""""""""""""""""""INSERT MODE MAPS"""""""""""""""""""
" paste
inoremap <C-v> <ESC>"+pa
" dictionary completion
inoremap <C-k> <C-x><C-x><C-k>
" allow us to undo accidental ctrl-u
inoremap <c-u> <c-g>u<c-u>
inoremap <c-w> <c-g>u<c-w>

""""""""""""""""""""""""NORMAL MODE MAPS"""""""""""""""""""
" Sends things away without overwriting the register
nnoremap <leader>d "_d

" Write
nnoremap <Esc> <Esc>:w<Enter>

" Remove search highlights
nnoremap <F3> :noh<CR>

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

function MoveToPrevTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() != 1
    close!
    if l:tab_nr == tabpagenr('$')
      tabprev
    endif
    sp
  else
    close!
    exe "0tabnew"
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

function MoveToNextTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() < tab_nr
    close!
    if l:tab_nr == tabpagenr('$')
      tabnext
    endif
    sp
  else
    close!
    tabnew
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

nnoremap mt :call MoveToNextTab()<CR>
nnoremap mT :call MoveToPrevTab()<CR>

"""""""""""""""""""""VISUAL MODE MAPS""""""""""""""""""""""
" Sends things away without overwriting the register
vnoremap <leader>d "_d

" Move selected blocks up and down
vnoremap <A-j> :m '>+1<CR>gv
vnoremap <A-k> :m '<-2<CR>gv

vnoremap <C-c> "+y
vnoremap <C-x> "+d

""""""""""""""""""""""""""""MISC""""""""""""""""""""""""""
