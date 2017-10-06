call plug#begin('~/.vim/plugged')

Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/syntastic'
Plug 'sheerun/vim-polyglot'
Plug 'skwp/vim-colors-solarized'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'vimoutliner/vimoutliner'

call plug#end()

set hlsearch
set ignorecase
set smartcase

set noswapfile
set nobackup
set nowritebackup

set hidden
set noerrorbells
set lazyredraw

set clipboard=unnamedplus
set spelllang=en_us
set spell

set background=dark
let g:solarized_termcolors=16
colorscheme solarized

set expandtab
set tabstop=2
set softtabstop=2

set mouse=a
set number
set colorcolumn=80

" Easier path expansion.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Remove trailing whitespace on save.
autocmd BufWritePre *.* :%s/\s\+$//e

autocmd FileType markdown,text,rmd setlocal tw=79

map <space> \

" fzf mappings.
nmap <Leader>e :Files<CR>
nmap <Leader>b :Buffers<CR>
nmap <Leader>a :Ag<CR>
nmap <Leader>/ :BLines<CR>

" Allow sending code to other tmux pane.
let g:slime_target = "tmux"

" Syntastic checkers.
let g:go_fmt_command = "goimports"
let g:syntastic_go_checkers= ['go', 'gofmt','golint', 'gotype', 'govet']
