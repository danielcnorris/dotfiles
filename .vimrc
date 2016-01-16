" Daniel Norris's vimrc
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'skwp/vim-colors-solarized'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-speeddating'
Plugin 'scrooloose/syntastic'
Plugin 'jiangmiao/auto-pairs'
call vundle#end()
filetype plugin indent on
runtime macros/matchit.vim

set number
set ruler
set laststatus=2
set encoding=utf-8
set backspace=indent,eol,start
set history=1000
set showcmd
set showmode
set visualbell
set noerrorbells
set lazyredraw
set autochdir
set autoread
set hidden
set showmatch
set nrformats=

<<<<<<< HEAD
let mapleader=" "
=======
" Basic mappings
let mapleader=","
nnoremap ' `
nnoremap ` '
inoremap jk <Esc>
"noremap ; :
nnoremap j gj
nnoremap k gk

" Load Vundle modules
if filereadable(expand("~/.vim/vundles.vim"))
  source ~/.vim/vundles.vim
endif
>>>>>>> 3afa9e810f3843af08f7b60defbfb1e6a0d1d35d

syntax on
set background=dark
try
  colorscheme solarized
catch
endtry

set undodir=~/.vim/backups
set undofile
set noswapfile
set nobackup
set nowritebackup

set autoindent
set smartindent
set expandtab
set shiftwidth=2
set softtabstop=2
set textwidth=79
set colorcolumn=80
set hlsearch
set incsearch
nmap <silent> <Leader><Leader> :nohlsearch<CR>

" Remove trailing whitespace on save
autocmd BufWritePre *.* :%s/\s\+$//e

" Turnoff auto comment insertion
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
<<<<<<< HEAD
=======

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
nmap <silent> <Leader>/ :nohlsearch<CR>

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
nnoremap <Leader>a :bp<CR>
nnoremap <Leader>s :bn<CR>
nnoremap <Leader>x :bp\|bd #<CR>

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

" Stylish haskell
command Style %!stylish-haskell

set nrformats=
>>>>>>> 3afa9e810f3843af08f7b60defbfb1e6a0d1d35d
