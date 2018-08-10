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

let g:go_fmt_command = "goimports"
let g:go_metalinter_autosave = 1

set grepprg=rg\ --vimgrep
command! -nargs=+ Grep silent! grep! <args> | silent redraw!

" Easier path expansion.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Remove trailing whitespace on save.
autocmd BufWritePre *.* :%s/\s\+$//e

let g:polyglot_disabled = ['python-compiler']

augroup Linting
	autocmd!
	autocmd FileType python setlocal makeprg=$HOME/dotfiles/python.sh
	" autocmd FileType go setlocal makeprg=$HOME/dotfiles/go.sh
	autocmd BufWritePost *.py silent make! <afile> | silent redraw!
	autocmd FileType javascript setlocal makeprg=$HOME/dotfiles/javascript.sh
	autocmd BufWritePost *.js silent make! <afile> | silent redraw!
augroup END
