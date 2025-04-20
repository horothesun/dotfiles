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
set cursorline
set noshowmatch
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

set splitbelow splitright

" disable unsafe commands
set secure

" do not keep a backup file (some LSP don't work well in coc with backup files)
set nobackup
set nowritebackup

" ~/.vim/undodir must be manually created
set undodir=~/.vim/undodir
set undofile

set incsearch
set termguicolors
set scrolloff=8

" Give more space for displaying messages
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable delays and poor user experience
set updatetime=50

" Don't pass messages to |ins-completion-menu|
set shortmess+=c

" Disable mouse integration
set mouse=

set colorcolumn=120
highlight ColorColumn ctermbg=0 guibg=lightgrey

call plug#begin('~/.vim/plugged')

Plug 'preservim/nerdcommenter'
" Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }
" Plug 'vimwiki/vimwiki'

" Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
" Plug 'junegunn/fzf.vim'

" Plug 'unisonweb/unison', { 'branch': 'trunk', 'rtp': 'editor-support/vim' }

" Telescope
" Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate c lua vim' }
" Plug 'nvim-lua/plenary.nvim'
" Plug 'nvim-telescope/telescope.nvim'
" Plug 'nvim-telescope/telescope-fzy-native.nvim'

Plug 'lewis6991/gitsigns.nvim'

Plug 'nvim-lua/plenary.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'scalameta/nvim-metals'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzy-native.nvim'
Plug 'hrsh7th/nvim-cmp'              " Core completion framework
Plug 'hrsh7th/cmp-nvim-lsp'          " LSP completion source
Plug 'L3MON4D3/LuaSnip'              " Lua-based snippet engine
Plug 'saadparwaiz1/cmp_luasnip'      " LuaSnip completion source
Plug 'j-hui/fidget.nvim'             " Neovim notifications and LSP progress messages
Plug 'ray-x/lsp_signature.nvim'      " show function signature when typing

" Color schemes
" Plug 'gruvbox-community/gruvbox'
Plug 'sainnhe/gruvbox-material'
" Plug 'phanviet/vim-monokai-pro'
" Plug 'flazz/vim-colorschemes'

call plug#end()



" \ 'coc-go',
" \ 'coc-metals',
" \ 'coc-tsserver',
" let g:coc_global_extensions = [
"   \ 'coc-json',
"   \ 'coc-markdownlint',
"   \ 'coc-sh',
"   \ 'coc-sumneko-lua'
"   \ ]

" VimWiki
" let g:vimwiki_list = [
"   \ { 'path': '~/vimwiki/personal/', 'syntax': 'markdown', 'ext': '.md' },
"   \ { 'path': '~/vimwiki/work/',     'syntax': 'markdown', 'ext': '.md' }
"   \ ]
" let g:vimwiki_global_ext = 0
" let g:vimwiki_markdown_link_ext = 1


" let g:gruvbox_contrast_dark='hard'
" colorscheme gruvbox
" set background=dark

set background=dark
let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_better_performance = 1
colorscheme gruvbox-material


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


" Help Vim recognize *.sbt and *.sc as Scala files
au BufRead,BufNewFile *.sbt,*.sc,*.scala set filetype=scala

" Help Vim recognize *.tf as Terraform files
au BufRead,BufNewFile *.tf set filetype=tf

if executable('rg')
  let g:rg_derive_root='true'
endif

let loaded_matchparen = 1
let mapleader = " "

let g:netrw_browse_split = 2
let g:vrfr_rg = 'true'
let g:netrw_banner = 0
let g:netrw_winsize = 25

nnoremap <leader>pv :topleft wincmd v<bar> :Ex <bar> :vertical resize 30<CR>


" replace all
nnoremap <leader>s :%s//gc<Left><Left><Left>


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
    -- vimgrep_arguments = {
    --   'rg',
    --   '--color=never',
    --   '--no-heading',
    --   '--with-filename',
    --   '--line-number',
    --   '--column',
    --   '--smart-case',
    --   '--unrestricted', -- disable .gitignore handling
    --   '--unrestricted'  -- search hidden files and directories
    -- },
    layout_config = { width = 0.9, height = 0.9 },
    mappings = {
      i = {
        ['<esc>'] = require('telescope.actions').close,
      }
    },
    file_ignore_patterns = {
      ".git/",
      ".idea/",
      ".metals/",
      ".bloop/",
      "node_modules/"
    }
  },
  pickers = {
    find_files = { hidden = true },
    live_grep = {
      additional_args = function(opts)
        return { "--hidden" }
      end
    }
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


" gitsigns
lua <<EOF
require('gitsigns').setup()
EOF


" Telescope remaps
nnoremap <C-p> <cmd>Telescope live_grep<CR>
nnoremap <leader>ff <cmd>Telescope find_files<CR>
nnoremap <leader>fb <cmd>Telescope buffers<CR>
nnoremap <leader>fh <cmd>Telescope help_tags<CR>
nnoremap <leader>fe <cmd>luafile ~/.config/nvim/pickers/emoji_picker.lua<CR>
nnoremap <leader>fs <cmd>luafile ~/.config/nvim/pickers/symbol_picker.lua<CR>


nnoremap <leader>h :wincmd h<CR>
nnoremap <leader>j :wincmd j<CR>
nnoremap <leader>k :wincmd k<CR>
nnoremap <leader>l :wincmd l<CR>
nnoremap <leader>+ :vertical resize +5<CR>
nnoremap <leader>- :vertical resize -5<CR>

inoremap <C-c> <esc>


" TODO: replace with native LSP commands
"
" GoTo code navigation
" nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)
" nmap <leader>rn <Plug>(coc-rename)
" nmap <silent> g[ <Plug>(coc-diagnostic-prev)
" nmap <silent> g] <Plug>(coc-diagnostic-next)
"
"nmap <silent> <leader>gp <Plug>(coc-diagnostic-prev-error)
"nmap <silent> <leader>gn <Plug>(coc-diagnostic-next-error)

" TODO: replace with native LSP commands
"
" Fix autofix problem of current line
" nmap <leader>qf <Plug>(coc-fix-current)

" TODO: replace with native LSP commands
"
" Remap for do codeAction of current line
" xmap <leader>a <Plug>(coc-codeaction-line)
" nmap <leader>a <Plug>(coc-codeaction-line)

" TODO: replace with native LSP commands
"
" Use K to either doHover or show documentation in preview window
" nnoremap <silent> K :call <SID>show_documentation()<CR>
" function! s:show_documentation()
"   if (index(['vim','help'], &filetype) >= 0)
"     execute 'h '.expand('<cword>')
"   else
"     call CocAction('doHover')
"   endif
" endfunction


" TODO: replace with native LSP commands
"
" Customise autocomplete popup menu behaviour
" inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm() : "\<CR>"
" inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
" inoremap <silent><expr> <S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"


" nnoremap <leader>cr :CocRestart<CR>


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

" Enable NERDCommenterToggle to check all selected lines are commented or not
let g:NERDToggleCheckAllLines = 1

nmap <leader>cc <Plug>NERDCommenterToggle
vmap <leader>cc <Plug>NERDCommenterToggle

" Load Lua setup
lua require('setup1')
