call plug#begin('~/.vim/plugged')

Plug 'Shougo/deoplete.nvim'
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'airblade/vim-gitgutter'
Plug 'carlitux/deoplete-ternjs', { 'do': 'npm install -g tern' }
Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'honza/vim-snippets'
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
Plug 'zchee/deoplete-go', { 'do': 'make'}
Plug 'zchee/deoplete-jedi'

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

map <space> \

" FZF configuration.
nmap <leader>e :Files<CR>
nmap <leader>b :History<CR>
nmap <leader>a :Ag<CR>
nmap <leader>s :BLines<CR>

" Go configuration.
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_auto_type_info = 1
let g:go_auto_sameids = 1

autocmd FileType go nmap <leader>t  <Plug>(go-test)
autocmd FileType go nmap <Leader>c <Plug>(go-coverage-toggle)
autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')

" Ale configuration.
let g:ale_fixers = {
\   'python': ['yapf'],
\   'javascript': ['prettier', 'eslint'],
\}

let g:ale_linters = {
\   'go': ['go vet', 'golint', 'gometalinter', 'go build'],
\}
let g:ale_go_metalinter_options = '--fast'
let g:ale_fix_on_save = 1

" Autocompletion and snippets.
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)
set completeopt-=preview

" Send code to Tmux pane.
let g:slime_target = "tmux"
