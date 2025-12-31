" Use Vim settings rather than Vi settings.
" This must be first, because it changes other options as a side effect.
set nocompatible

" Enable file detection, plugin and indentation
filetype plugin indent on

" Switch syntax highlighting on (requires filetype detection on)
syntax on

" Neovim clipboard integration
set clipboard=unnamedplus

" Optimize for fast terminal connections
set ttyfast

" Set encoding to UTF-8 without BOM
set encoding=utf-8 nobomb

" Show invisible characters and set indentation preferences
set list
set lcs=tab:▸\ ,trail:·,nbsp:_
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab smartindent

" show encoding in statusbar, if/when statusbar is enabled
if has("statusline")
  " https://shapeshed.com/vim-statuslines/
  set statusline=
  " set statusline+=%#PmenuSel#
  " set statusline+=%{mode()}
  set statusline+=%#LineNr#
  set statusline+=\ %f
  set statusline+=%m
  set statusline+=\ %=
  set statusline+=%#CursorColumn#
  set statusline+=\ %l:%c\ (%p%%)\ \|
  set statusline+=\ %y\ \|
  set statusline+=\ %{&fileencoding?&fileencoding:&encoding}
  set statusline+=\[%{&fileformat}\]\ |
endif

" no statusbar by default (0 = never, 2 = always)
set laststatus=2

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
"set fillchars+=eob:\

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

" It seems to fix the issue with nvim-metals which requires to "Press ENTER..." to continue after indexing.
" E.g. similar issue here: https://neovim.discourse.group/t/how-to-hide-press-enter-or-type-to-continue/3147
"
" N.B.: nvim 0.12.0 will include a solution to this: https://github.com/neovim/neovim/pull/27855
"
" Give more space for displaying messages.
set cmdheight=2

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

" ==========================
" Vim-Plug Setup
" ==========================
" https://github.com/junegunn/vim-plug
" Installation of vim-plug is described in the readme (curl command)
" curl -fLo $HOME/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
" Restart and run :PlugInstall to install plugins.
" To uninstall, remove it from this file and run :PlugClean

call plug#begin('~/.config/nvim/plugged')

Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }
Plug 'lewis6991/gitsigns.nvim'
Plug 'lewis6991/spaceless.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'scalameta/nvim-metals'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
Plug 'nvim-telescope/telescope-symbols.nvim'
Plug 'hrsh7th/nvim-cmp'         " Core completion framework
Plug 'hrsh7th/cmp-nvim-lsp'     " LSP completion source
Plug 'L3MON4D3/LuaSnip'         " Lua-based snippet engine
Plug 'saadparwaiz1/cmp_luasnip' " LuaSnip completion source
Plug 'tpope/vim-projectionist'  " Go-to-tests
Plug 'Mrcjkb/haskell-tools.nvim', { 'version': 6, 'for': ['haskell'] }
" Plug 'kana/vim-textobj-user', { 'for': ['agda'] }
" Plug 'neovimhaskell/nvim-hs.vim', { 'for': ['agda'] }
" Plug 'agda/cornelis', { 'for': ['agda'], 'do': 'stack build' }
Plug 'j-hui/fidget.nvim' " Neovim notifications and LSP progress messages

" Color schemes
" Plug 'gruvbox-community/gruvbox'
Plug 'sainnhe/gruvbox-material'

call plug#end()

" Default colorscheme (has to be installed, see vim-plug above)
" Place this code AFTER the vim-plug section, otherwise you need to generate symb links in the colors folder
set termguicolors " enable true colors support
set background=dark
let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_better_performance = 1
colorscheme gruvbox-material

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

nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>+ :vertical resize +5<CR>
nnoremap <leader>- :vertical resize -5<CR>

inoremap <C-c> <esc>

" Disable arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>

" iamcco/markdown-preview
" CSS: https://github.com/sindresorhus/github-markdown-css/blob/gh-pages/github-markdown.css
let g:mkdp_auto_close=0
let g:mkdp_refresh_slow=1
" let g:mkdp_markdown_css=expand('~/github-markdown.css')

" Persist Undo in an XDG-Compliant Location
if !isdirectory($HOME."/.local/share/nvim/undo")
    call mkdir($HOME."/.local/share/nvim/undo", "p", 0700)
endif
set undodir=~/.local/share/nvim/undo
set undofile

" Load Lua setup
lua require('setup1')
