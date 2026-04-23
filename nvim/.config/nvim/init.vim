" Use Vim settings rather than Vi settings.
" This must be first, because it changes other options as a side effect.
set nocompatible

" Enable file detection, plugin and indentation
filetype plugin indent on

" Switch syntax highlighting on (requires filetype detection on)
syntax on

" Optimize for fast terminal connections
set ttyfast

" Set encoding to UTF-8 without BOM
set encoding=utf-8 nobomb

" Show invisible characters and set indentation preferences
set list
set lcs=tab:▸\ ,trail:␣,nbsp:¬
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab autoindent

" stop the indent jump while typing `>` in scala buffers
autocmd FileType scala setlocal indentkeys=0{,0},0),!^F,o,O,e,=case,<CR>

" show encoding in statusbar, if/when statusbar is enabled
set laststatus=3 " 0 = never, 2 = always, 3 = global for all windows
if has("statusline")
  " https://shapeshed.com/vim-statuslines/
  set statusline=
  " set statusline+=%#PmenuSel#
  " set statusline+=%{mode()}
  set statusline+=%#StatusLineNC#
  "set statusline+=\ %f
  set statusline+=%m
  set statusline+=\ %=
  "set statusline+=%#CursorColumn#
  set statusline+=\ %l:%c\ (%p%%)\ \|
  set statusline+=\ %y\ \|
  set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
  set statusline+=\[%{&fileformat}\]\ |
endif

" Do not add empty newline at EOF
set noeol

set cursorline
set noshowmatch
set nohlsearch
set hidden
set nowrap
set ignorecase
set smartcase
set noswapfile
set incsearch

set splitbelow splitright

set colorcolumn=120
highlight ColorColumn ctermbg=0 guibg=lightgrey

" Display end of buffer lines as blank
set fillchars+=eob:\  " eob fillchar is a space; keep the escaped space before this comment

" Disable unsafe commands and the ruler display
set secure
set noruler

" Show line numbers
set number

" Disable mouse support
set mouse=

" Disable error bells
set noerrorbells
set belloff=all

" Milliseconds after stop typing before processing plugins (default 4000)
set updatetime=50
set scrolloff=8
set sidescrolloff=5
set sidescroll=1

" Do not keep backup files (some LSPs are sensitive to backup files)
set nobackup
set nowritebackup

" Display incomplete commands while typing
set showcmd

" Fix the asymmetry between Ctrl-W n and Ctrl-W v to split the window
nnoremap <C-w>v :vnew<CR>

" Remove search highlights when a search is completed
"nnoremap <leader>h :nohlsearch<CR>

" Do not change the cursor shape in insert mode
set guicursor=

if executable('rg')
  let g:rg_derive_root='true'
endif

let loaded_matchparen = 1
let mapleader = " "
nnoremap <Space> <Nop>
vnoremap <Space> <Nop>

let g:netrw_browse_split = 2
let g:vrfr_rg = 'true'
let g:netrw_banner = 0
let g:netrw_winsize = 25

nnoremap <leader>pv :topleft wincmd v<bar> :Ex <bar> :vertical resize 30<CR>

" Keep previously yanked text in the registry
"
" Paste in VISUAL-mode
xnoremap <leader>p "_dP
" Delete in NORMAL-mode
nnoremap <leader>d "_d
" Delete in VISUAL/SELECT-mode
vnoremap <leader>d "_d

" Replace all
nnoremap <leader>s :%s//gc<Left><Left><Left>

" Make Y behave like C and D
nnoremap Y y$

" Keeping it centered
nnoremap n nzz
nnoremap N Nzz
nnoremap <C-d> <C-d>zz
nnoremap <C-u> <C-u>zz

" undo break points
inoremap , ,<C-g>u
inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u

nnoremap <leader>H :wincmd h<CR>
nnoremap <leader>J :wincmd j<CR>
nnoremap <leader>K :wincmd k<CR>
nnoremap <leader>L :wincmd l<CR>
nnoremap <leader>+ :vertical resize +5<CR>
nnoremap <leader>- :vertical resize -5<CR>

inoremap <C-c> <esc>

" Disable arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" Persist Undo in an XDG-Compliant Location
if !isdirectory($HOME."/.local/share/nvim/undo")
    call mkdir($HOME."/.local/share/nvim/undo", "p", 0700)
endif
set undodir=~/.local/share/nvim/undo
set undofile

" Load Lua setup
lua require('setup1')
