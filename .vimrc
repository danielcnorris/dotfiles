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
set smartindent
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set textwidth=79
set colorcolumn=80
set hlsearch
set incsearch
nmap <silent> <Leader><Leader> :nohlsearch<CR>

" Remove trailing whitespace on save
autocmd BufWritePre *.* :%s/\s\+$//e

" Turnoff auto comment insertion
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
