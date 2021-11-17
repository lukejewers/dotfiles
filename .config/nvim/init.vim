" plugs 
call plug#begin('~/.vim/plugged')
Plug 'navarasu/onedark.nvim'
Plug 'itchyny/lightline.vim'
Plug 'itchyny/vim-gitbranch'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-fzf-native.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'szw/vim-maximizer'
Plug 'christoomey/vim-tmux-navigator'
Plug 'sbdchd/neoformat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'neovim/nvim-lspconfig'
call plug#end()

" leader
let mapleader="\<Space>" 

" default sets 
syntax enable
filetype plugin indent on
set completeopt=menu,menuone,noselect
set mouse=a
set splitright
set splitbelow
set noerrorbells
set belloff=all
set shiftwidth=2
set tabstop=2 softtabstop=2
set expandtab
set smartindent
set linebreak
set nu
set wrap
set smartcase
set noswapfile
set cmdheight=1
set nobackup
set undodir=~/.vim/undodir
set relativenumber
set incsearch
colorscheme onedark 
nnoremap <leader>vrc :e $MYVIMRC<CR>

" netrw
let g:netrw_banner=0
let g:netrw_winsize=25
nnoremap <leader>nt :Vexplore<CR>

" itchyny/vim-gitbranch itchyny/lightline
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'gitbranch#name'
      \ },
      \ }

" szw/vim-maximizer
nnoremap <leader>m :MaximizerToggle!<CR>

" sbdchd/neoformat
nnoremap <leader>fmt :Neoformat<CR>

" nvim-telescope/telescope.nvim
" Using Lua functions
nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>


" langserver
lua <<EOF
  require'lspconfig'.html.setup{}
  require'lspconfig'.eslint.setup{}
  require'lspconfig'.tsserver.setup{}
  require'lspconfig'.denols.setup{}
  require'lspconfig'.rust_analyzer.setup{}
  require'lspconfig'.hls.setup{}
EOF
