call plug#begin('~/.vim/plugged')

Plug 'SirVer/ultisnips'
Plug 'Shougo/deoplete.nvim'
Plug 'airblade/vim-gitgutter'
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'jpalardy/vim-slime'
Plug 'jparise/vim-graphql'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-plug'
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
Plug 'sheerun/vim-polyglot'
Plug 'skwp/vim-colors-solarized'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'w0rp/ale'

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

set clipboard=unnamed,unnamedplus
set spelllang=en_us
set spell

set background=dark
try
  colorscheme solarized
catch
endtry
let g:solarized_termcolors=16

set mouse=a
set number
set colorcolumn=80

" Easier path expansion.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
" Remove trailing whitespace on save.
autocmd BufWritePre *.* :%s/\s\+$//e

autocmd FileType markdown,text,rmd setlocal tw=79

map <space> \

" FZF mappings.
nmap <leader>e :Files<CR>
nmap <leader>e :Files<CR>
nmap <leader>f :History<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>a :Ag<CR>
nmap <leader>s :BLines<CR>

let g:slime_target = "tmux"

let g:go_fmt_command = "goimports"

let g:ale_fixers = {
\   'python': ['yapf'],
\   'javascript': ['prettier', 'eslint'],
\}

let g:ale_linters = {
\   'go': ['go vet', 'golint', 'gometalinter', 'go build'],
\}
let g:ale_go_metalinter_options = '--fast'
let g:ale_fix_on_save = 1
let g:deoplete#enable_at_startup = 1
