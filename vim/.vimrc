" Reference: https://github.com/erkrnt/awesome-streamerrc

" Use Vim settings, rather then Vi settings (much better!).
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

" performance improvements when syntax on in vim 8+
if v:version >= 800
  syntax sync minlines=256
endif

" set term=xterm-256color
set noeol
set encoding=utf-8 nobomb
set guicursor=
set noshowmatch
set relativenumber
set nohlsearch
set hidden
set noerrorbells belloff=all
set tabstop=2 softtabstop=2 shiftwidth=2 expandtab
set smartindent
set number
set nowrap
set ignorecase
set smartcase
set noswapfile

" disable unsafe commands
set secure

" do not keep a backup file (some LSP don't work well in coc with backup files)
set nobackup
set nowritebackup

" ~/.vim/undodir must be manually created
set undodir=~/.vim/undodir
set undofile

" Spelunker: disable vim's spell check
" set nospell

set incsearch
set termguicolors
set scrolloff=8

" Give more space for displaying messages
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable delays and poor user experience
set updatetime=50

" Don't pass messages to |ins-completion-menu|
set shortmess+=c

set colorcolumn=110
highlight ColorColumn ctermbg=0 guibg=lightgrey

call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }

" Telescope
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'

" Spelunker spell checker
" Plug 'kamykn/spelunker.vim'
" Plug 'kamykn/popup-menu.nvim'

" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Plug 'junegunn/fzf.vim'

" Color schemes
Plug 'gruvbox-community/gruvbox'
Plug 'sainnhe/gruvbox-material'
Plug 'phanviet/vim-monokai-pro'
Plug 'vim-airline/vim-airline'
Plug 'flazz/vim-colorschemes'

call plug#end()


let g:coc_global_extensions = [
  \ 'coc-sh',
  \ 'coc-json',
  \ 'coc-tsserver',
  \ 'coc-markdownlint',
  \ 'coc-metals'
  \ ]


" Help Vim recognize *.sbt and *.sc as Scala files
au BufRead,BufNewFile *.sbt,*.sc,*.scala set filetype=scala


let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox
set background=dark

if executable('rg')
  let g:rg_derive_root='true'
endif

let loaded_matchparen = 1
let mapleader = " "

let g:netrw_browse_split = 2
let g:vrfr_rg = 'true'
let g:netrw_banner = 0
let g:netrw_winsize = 25

"nnoremap <leader>pv :wincmd v<bar> :Ex <bar> :vertical resize 30<CR>
nnoremap <leader>pv :NERDTree<CR>


" make Y behave like C and D
nnoremap Y y$


" keeping it centered
" nnoremap n nzz
" nnoremap N Nzz
" nnoremap J mzJ`z


" undo break points
inoremap , ,<C-g>u
inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u


" fzf
" nnoremap <C-p> :Rg<CR>
" nnoremap <leader>ff :GFiles --cached --others --exclude-standard<CR>
" nnoremap <leader>fb :Buffers<CR>
" let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.94, 'relative': v:true, 'yoffset': 0.9 } }
" let g:fzf_preview_window = ['right:50%']

" Telescope
lua <<EOF
require('telescope').setup {
  defaults = {
    file_sorter = require('telescope.sorters').get_fzy_sorter,
    prompt_prefix = '> ',
    color_devicons = true,
    file_previewer = require('telescope.previewers').vim_buffer_cat.new,
    grep_previewer = require('telescope.previewers').vim_buffer_vimgrep.new,
    qflist_previewer = require('telescope.previewers').vim_buffer_qflist.new,
    layout_config = { width = 0.9, height = 0.9 },
    mappings = {
      i = {
        ['<esc>'] = require('telescope.actions').close,
      }
    },
    file_ignore_patterns = {
      ".git",
      "node_modules"
    }
  },
  pickers = {
    find_files = { hidden = true }
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true
    }
  }
}
require('telescope').load_extension('fzy_native')
EOF

" Telescope remaps
nnoremap <C-p> <cmd>Telescope live_grep<CR>
nnoremap <leader>ff <cmd>Telescope find_files<CR>
nnoremap <leader>fb <cmd>Telescope buffers<CR>
nnoremap <leader>fh <cmd>Telescope help_tags<CR>
nnoremap <leader>fe <cmd>luafile ~/.config/nvim/emojiPicker.lua<CR>


nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>+ :vertical resize +5<CR>
nnoremap <leader>- :vertical resize -5<CR>

inoremap <C-c> <esc>

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction


" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Used in the tab autocompletion for coc
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"


" GoTo code navigation
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <leader>rn <Plug>(coc-rename)
nmap <silent> g[ <Plug>(coc-diagnostic-prev)
nmap <silent> g] <Plug>(coc-diagnostic-next)
"nmap <silent> <leader>gp <Plug>(coc-diagnostic-prev-error)
"nmap <silent> <leader>gn <Plug>(coc-diagnostic-next-error)

" Fix autofix problem of current line
nmap <leader>qf <Plug>(coc-fix-current)

" Remap for do codeAction of current line
xmap <leader>a <Plug>(coc-codeaction-line)
nmap <leader>a <Plug>(coc-codeaction-line)

" Use K to either doHover or show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Notify coc.nvim that <enter> has been pressed.
" Currently used for the formatOnType feature.
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
  \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

nnoremap <leader>cr :CocRestart<CR>


" Disable arrow keys
noremap <Up> <Nop>
noremap <Down> <Nop>
noremap <Left> <Nop>
noremap <Right> <Nop>


" White-space auto-trimming
fun! TrimWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfun
autocmd BufWritePre * :call TrimWhitespace()


" iamcco/markdown-preview
" CSS: https://github.com/sindresorhus/github-markdown-css/blob/gh-pages/github-markdown.css
let g:mkdp_auto_close=0
let g:mkdp_refresh_slow=1
" let g:mkdp_markdown_css=expand('~/github-markdown.css')


" NERDCommenter

" Create default mappings
let g:NERDCreateDefaultMappings = 0

" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" Use compact syntax for prettified multi-line comments
let g:NERDCompactSexyComs = 1

" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'

" Allow commenting and inverting empty lines (useful when commenting a region)
let g:NERDCommentEmptyLines = 1

" Enable NERDCommenterToggle to check all selected lines is commented or not
let g:NERDToggleCheckAllLines = 1

nmap <leader>cc <Plug>NERDCommenterToggle
vmap <leader>cc <Plug>NERDCommenterToggle
