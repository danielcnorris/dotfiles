call plug#begin('~/.vim/plugged')

Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'scrooloose/syntastic'
Plug 'sheerun/vim-polyglot'
Plug 'skwp/vim-colors-solarized'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'vimoutliner/vimoutliner'

call plug#end()

set hlsearch
set ignorecase
set smartcase

set noswapfile
set nobackup
set nowritebackup

set spelllang=en_us
set spell

set background=dark
let g:solarized_termcolors=16
colorscheme solarized

set mouse=a
set number
set colorcolumn=80
autocmd FileType markdown,text,rmd setlocal tw=79

map <space> \

" FZF mappings.
nmap <Leader>e :Files<CR>
nmap <Leader>b :Buffers<CR>
nmap <Leader>a :Ag<CR>
nmap <Leader>/ :BLines<CR>

cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
