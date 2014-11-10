" Daniel Norris's vimrc

" Use Vim
set nocompatible

" General settings
set title
set number
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

" Basic mappings
let mapleader=","
nnoremap ' `
nnoremap ` '
inoremap jj <Esc>
noremap ; :
nnoremap j gj
nnoremap k gk

" Load Vundle modules
if filereadable(expand("~/.vim/vundles.vim"))
  source ~/.vim/vundles.vim
endif

" Colors
set background=dark
if filereadable(expand("~/.vim/bundle/vim-colors-solarized"))
    colorscheme solarized
endif

" Turn off swap files
set noswapfile
set nobackup
set nowb

" Persistent undo
if has('persistent_undo')
  silent !mkdir ~/vim/backups > /dev/null 2>&1
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

" Line length
set textwidth=79
set colorcolumn=80

" Remove trailing whitespace on save
autocmd BufWritePre *.* :%s/\s\+$//e

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
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h
set splitbelow
set splitright
nnoremap vv <C-w>v<CR>
nnoremap ss <C-w>s<CR>
nnoremap <Leader>z :bp<CR>
nnoremap <Leader>x :bn<CR>

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

" PUT THESE IN LANGUAGE SPECIFIC FILES
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
