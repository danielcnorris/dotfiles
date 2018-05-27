call plug#begin('~/.vim/plugged')

Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-plug'
Plug 'romainl/vim-qf'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'

call plug#end()

set hlsearch
set ignorecase
set smartcase

set noswapfile
set nobackup
set nowritebackup

set hidden
set lazyredraw
set belloff=all

set clipboard=unnamed,unnamedplus

set spelllang=en_us
set spell
hi clear SpellBad                                                
hi SpellBad cterm=underline                                      
hi clear SpellRare                                               
hi SpellRare cterm=underline                                     
hi clear SpellCap                                                
hi SpellCap cterm=underline                                      
hi clear SpellLocal
hi SpellLocal cterm=underline

" Easier path expansion.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Remove trailing whitespace on save.
autocmd BufWritePre *.* :%s/\s\+$//e

map <space> \

" FZF configuration.
" Create a command for using Ripgrep.
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always -g "!{node_modules,vendor}/*" '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)
nmap <leader>e :Files<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>f :Rg<CR>
nmap <leader>s :BLines<CR>

" Go configuration.
let g:go_fmt_command = "goimports"
" Run gometalinter --install
let g:go_metalinter_autosave = 1

autocmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
autocmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')

" ALE configuration. 
" Lint and format on save only.
let g:ale_fixers = {
\   'python': ['yapf'],
\}
let g:ale_fix_on_save = 1
let g:ale_open_list = 1
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 0
let g:ale_set_signs = 0
