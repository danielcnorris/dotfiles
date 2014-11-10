" Daniel Norris's vimrc

" Use Vim
set nocompatible
                
" General settings
set title
"set number
set ruler
set backspace=indent,eol,start
set history=1000
set showcmd
set showmode
set visualbell
set noerrorbells
set autoread

" Syntax highlighting
syntax on

" Allow buffers to exist in background
set hidden

" Move leader
let mapleader=","

" Move ` to '
nnoremap ' `
nnoremap ` '

" Map ; to : for commands
noremap ; :

" Allow easier up/down navigation on wrapped lines
nnoremap j gj
nnoremap k gk

" Load Vundle modules
if filereadable(expand("~/.vim/vundles.vim"))
  source ~/.vim/vundles.vim
endif

" Colors
set background=dark
colorscheme solarized

" Turn off swap files
set noswapfile
set nobackup
set nowb

" Persistent undo
if has('persistent_undo')
  silent !mkdir ~/vim/backups > /dev/null 2 > &1
  set undodir=~/.vim/backups
  set undofile
endif

" Indentation
set autoindent
set expandtab
set shiftwidth=4
set softtabstop=4
set shiftround

filetype on
filetype plugin on
filetype indent on

" Display tabs and trailing whitespace
" set list listchars=tab:\ \ ,trail:·

" Turnoff auto comment insertion
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" set nowrap
" set linebreak

" Completion
set wildmode=longest,list,full
set wildmenu

" Scrolling context
" set scrolloff=3
" set sidescrolloff=5
" set sidescroll=1

" Highlight search
set hlsearch
set incsearch
set ignorecase
set smartcase
nmap <silent> <leader>/ :nohlsearch<CR>

" Closing character
set showmatch

" Statusline
set laststatus=2
set encoding=utf-8

" Buffer navigation
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <C-h> <C-w>h

nnoremap <Leader>1 :1b<CR>
nnoremap <Leader>2 :2b<CR>
nnoremap <Leader>3 :3b<CR>
nnoremap <Leader>4 :4b<CR>
nnoremap <Leader>5 :5b<CR>
nnoremap <Leader>6 :6b<CR>
nnoremap <Leader>7 :7b<CR>
nnoremap <Leader>8 :8b<CR>
nnoremap <Leader>9 :9b<CR>
nnoremap <Leader>10 :10b<CR>

""" BELOW HERE ALL SHOULD BE MOVED
" " Python standards as per http://docs.python.org/
" " en/latest/dev/env/#text-editors
au FileType python setlocal textwidth=79
au FileType python setlocal colorcolumn=80
highlight ColorColoumn ctermbg=black guibg=black
set shiftwidth=1
set tabstop=4
set expandtab
set softtabstop=4
set autoindent


" Remove trailing whitespace form python files
autocmd BufWritePre *.py :%s/\s\+$//e

" Give a shortcut to NERD tree
map <F2> :NERDTreeToggle<CR>

" Markdown syntax highlighting
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.md,*.txt setlocal spell spelllang=en_us

" Use Haskell mode for Elm files
autocmd BufNewFile,BufReadPost *.elm set filetype=haskell

" Haskell pointfree
autocmd BufEnter *.hs set formatprg=xargs\ pointfree

" ghc-mod
map <silent> tu :call GHC_BrowseAll()<CR>
map <silent> tw :call GHC_ShowType(1)<CR>

" hdevtools
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>
au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsInfo<CR>
