call plug#begin('~/.vim/plugged')

Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'jpalardy/vim-slime'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-plug'
Plug 'sheerun/vim-polyglot'
Plug 'skwp/vim-colors-solarized'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'vimoutliner/vimoutliner'
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

if has('macunix')
  set background=light
else
  set background=dark
endif
try
  colorscheme solarized
catch
endtry
let g:solarized_termcolors=16

set expandtab
set shiftwidth=2
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

" FZF mappings.
" Use FZF to search for a directory and then create a file in it.
function! s:append_dir_with_fzf(line)
  call fzf#run(fzf#wrap({
    \ 'options': ['--prompt', a:line.'> '],
    \ 'source': 'fd -t d',
    \ 'sink': {line -> feedkeys("\<esc>:e ".line."/", 'n')}}))
  return ''
endfunction

command! -nargs=* EditInDir call s:append_dir_with_fzf(<f-args>)

nmap <leader>e :Files<CR>
nmap <leader>g :EditInDir:<CR>
nmap <leader>e :Files<CR>
nmap <leader>e :Files<CR>
nmap <leader>f :History<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>a :Ag<CR>
nmap <leader>s :BLines<CR>

" Allow sending code to other tmux panes.
let g:slime_target = "tmux"

let g:go_fmt_command = "goimports"

let g:ale_fixers = {
\   'python': ['yapf'],
\}
let g:ale_fix_on_save = 1
