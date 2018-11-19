set hlsearch
set noswapfile
set nobackup
set nowritebackup
set hidden
set belloff=all
set clipboard=unnamed,unnamedplus
set mouse=a
set spelllang=en_us
set spell
highlight clear SpellBad
highlight SpellBad cterm=underline
highlight clear SpellRare
highlight SpellRare cterm=underline
highlight clear SpellCap
highlight SpellCap cterm=underline
highlight clear SpellLocal
highlight SpellLocal cterm=underline

let g:go_fmt_command = "goimports"
let g:go_metalinter_autosave = 1

set grepprg=rg\ --vimgrep
command! -nargs=+ Grep silent! grep! <args> | silent redraw!

" Easier path expansion.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Remove trailing whitespace on save.
autocmd BufWritePre * :%s/\s\+$//e

map <space> \

" FZF configuration.
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

" ALE configuration.
let g:ale_linters = {
\  'javascript': ['standard'],
\  'python': ['flake8'],
\}
let g:ale_fixers = {
\   'elm': ['elm-format'],
\   'python': ['black'],
\   'javascript': ['prettier', 'standard'],
\}
let g:ale_fix_on_save = 1
let g:ale_open_list = 1
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 0
let g:ale_set_signs = 0
let g:ale_linters_explicit = 1

let g:EditorConfig_exec_path = '/usr/local/bin/editorconfig'
let g:EditorConfig_core_mode = 'external_command'
