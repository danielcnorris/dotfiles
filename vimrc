set hlsearch
set noswapfile
set nobackup
set nowritebackup
set hidden
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

set grepprg=rg\ --vimgrep
command! -nargs=+ Grep silent! grep! <args> | silent redraw!

" Easier path expansion.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Remove trailing whitespace on save.
autocmd BufWritePre *.* :%s/\s\+$//e

augroup Linting
	autocmd!
	autocmd FileType python setlocal makeprg=$HOME/dotfiles/python.sh
	autocmd FileType go setlocal makeprg=$HOME/dotfiles/go.sh
	autocmd BufWritePost *.py,*.go silent make! <afile> | silent redraw!
augroup END
