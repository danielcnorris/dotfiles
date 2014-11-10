" Update using :PluginInstall or vim +PluginInstall +qall
filetype off

" Set runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.vim/vundles/ " Submodules
call vundle#rc()

Plugin 'gmarik/vundle'

" Install categorized vundle files
runtime appearance.vundle
runtime git.vundle
runtime languages.vundle
runtime project.vundle
runtime vim-improvements.vundle


" Required by Vundle
filetype plugin indent on
