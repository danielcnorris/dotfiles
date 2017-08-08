" Daniel Norris's vimrc
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'fatih/vim-go'
Plugin 'godlygeek/tabular'
Plugin 'jpalardy/vim-slime'
Plugin 'leafgarland/typescript-vim'
Plugin 'raichoo/purescript-vim'
Plugin 'scrooloose/syntastic'
Plugin 'skwp/vim-colors-solarized'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-ragtag'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'vimoutliner/vimoutliner'
call vundle#end()
runtime macros/matchit.vim
filetype plugin indent on

set number
set mouse=a
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
" Encryption
set cm=blowfish
" OSX copy paste
set clipboard=unnamed
set spelllang=en_us
let mapleader=" "

syntax on
set background=light
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
set shiftwidth=2
set softtabstop=2
set tabstop=2
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
autocmd BufRead,BufNewFile *.md setlocal tw=80 spell sw=4 softtabstop=4 tabstop=4
autocmd BufRead,BufNewFile *.Rmd setlocal tw=80 spell
autocmd BufRead,BufNewFile *.txt setlocal tw=80 spell
autocmd BufRead,BufNewFile *.otl setlocal spell sw=4 softtabstop=4 tabstop=4 fo+=cro
autocmd BufRead,BufNewFile *.py setlocal sw=4 softtabstop=4 tabstop=4
autocmd BufRead,BufNewFile *.go setlocal sw=4 softtabstop=4 tabstop=4
let g:slime_target = "tmux"
