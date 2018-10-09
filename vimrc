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

map <space> \

" FZF configuration.
" Create a command for using Ripgrep.
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
" Lint and format on save only.
let g:ale_linters = {
\  'javascript': ['standard'],
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
