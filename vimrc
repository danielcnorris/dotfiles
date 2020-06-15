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

" Easier path expansion.
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

map <space> \

" FZF configuration.
nmap <leader>e :Files<CR>
nmap <leader>b :Buffers<CR>
nmap <leader>g :Rg<CR>
nmap <leader>s :BLines<CR>

" ALE configuration.
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'go': ['gofmt', 'goimports'],
\   'javascript': ['prettier'],
\   'python': ['isort', 'black'],
\}
let g:ale_fix_on_save = 1
let g:ale_open_list = 1
let g:ale_set_loclist = 0
let g:ale_set_quickfix = 1
let g:ale_set_signs = 0
let g:ale_linters_explicit = 1
