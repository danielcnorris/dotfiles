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
Plugin 'godlygeek/tabular'
Plugin 'vimoutliner/vimoutliner'
call vundle#end()
runtime macros/matchit.vim
filetype plugin indent on

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
set autoread
set hidden
set showmatch
set nrformats=
" OSX copy paste
set clipboard=unnamed
set spell spelllang=en_us
let mapleader=" "

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
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set colorcolumn=80
set hlsearch
set incsearch
set ignorecase
set smartcase

nmap <silent> <Leader><Leader> :nohlsearch<CR>
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
nnoremap <C-J> o<Esc>k
nnoremap <C-K> O<Esc>j

" Remove trailing whitespace on save
autocmd BufWritePre *.* :%s/\s\+$//e

" Turnoff auto comment insertion
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
autocmd BufRead,BufNewFile *.md setlocal tw=80 spell
autocmd BufRead,BufNewFile *.txt setlocal tw=80 spell
autocmd FileType *.otl setlocal spell
