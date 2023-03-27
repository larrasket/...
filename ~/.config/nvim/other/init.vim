set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
"  |  \   |  \      |      \      |  \     /  \
"  | $$   | $$       \$$$$$$      | $$\   /  $$
"  | $$   | $$        | $$        | $$$\ /  $$$
"   \$$\ /  $$        | $$        | $$$$\  $$$$
"    \$$\  $$         | $$        | $$\$$ $$ $$
"     \$$ $$         _| $$_       | $$ \$$$| $$
"      \$$$         |   $$ \      | $$  \$ | $$
"       \$           \$$$$$$       \$$      \$$
"


" ---- Basic Settings ----------------------------------------------------------

set nocompatible                                " no vi compatibility
set shell=/usr/bin/zsh                          " set default shell
set noswapfile                                  " no swap files
syntax enable                                   " enable syntax
filetype plugin indent on
set mouse=a                                     " enable mouse
set updatetime=30                               " time before updates
set number relativenumber                       " line numbers
set history=1000                                " command history
set signcolumn=yes numberwidth=6                " signcolumn and available width
set linebreak                                   " text wrap wont break words
set textwidth=100                               " line length
"set spell                                       " spell check
set smartcase ignorecase incsearch              " searching and highlighting
set foldmethod=indent foldlevel=2 foldcolumn=2  " code folding
set foldlevelstart=99 foldopen+=jump nofoldenable " auto fold open/close
set clipboard=unnamedplus                       " clipboard register
set tabstop=8 softtabstop=0                     " tab settings
set expandtab shiftwidth=2 smarttab
set breakindent                                 " smart wrapping indentation
set breakindentopt=shift:2,min:40,sbr
set showbreak=>>                                " shows line break
set ruler                                       " always show cursor
set wrap                                        " enable text wrapping
set scrolloff=5                                 " lines above/below cursor when scrolling
set showcmd                                     " key strokes in command line
set splitbelow splitright                       " new windows split right/bottom
set wildmenu wildmode=longest:full,full         " command autocompletion
set timeoutlen=1000 ttimeoutlen=1000            " timeout for keybind presses
set autowrite                                   " auto save
set confirm                                     " prompt to save not error
set noerrorbells                                " no ring on error
set undofile undodir=~/.vim/undo-dir            " persistent undo
set listchars=tab:>-                            " no endline chars
set hidden                                      " hide buffers instead of close
set autochdir                                   " relative filepaths
set completeopt=noinsert                        " dont insert automatically in completion
if has("nvim")
  set termguicolors                             " better colors
endif


" ---- Plugins -----------------------------------------------------------------

call plug#begin('~/.config/nvim/plugged')
  " Status bar {
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
  " }
  " File tree {
    if has("nvim")
      Plug 'ms-jpq/chadtree', {'branch': 'chad', 'do': 'python3 -m chadtree deps'}
    endif
  " }
  " Commenting {
    Plug 'preservim/nerdcommenter'
  " }
  " Tags {
    Plug 'preservim/tagbar', { 'on': 'TagbarToggle' }
  " }
  " LSP {
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'pechorin/any-jump.vim'                " jump to definition/reference
    if has("nvim")
      Plug 'folke/trouble.nvim'                   " better loc list
      Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
      Plug 'ms-jpq/coq.artifacts', {'branch': 'artifacts'}
    endif
  " }
  " fuzzy finders {
    if has("nvim")
      Plug 'nvim-lua/plenary.nvim'
      Plug 'nvim-telescope/telescope.nvim'        " search files within vim
      Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    endif
    Plug 'ctrlpvim/ctrlp.vim'
  " }
  " Focus helpers {
    Plug 'junegunn/goyo.vim'
    Plug 'junegunn/limelight.vim'
  " }
  " Sessions {
    " Plug 'xolox/vim-misc'
    " Plug 'xolox/vim-session'                    " Save sessions
  " }
  " Lang Plugins {
    Plug 'lervag/vimtex'
    Plug 'wakatime/vim-wakatime'
    Plug 'rebelot/kanagawa.nvim'
  " }
  " Git {
    Plug 'tpope/vim-fugitive'                   " Git inside vim
    Plug 'airblade/vim-gitgutter'               " show changes in signcolumn
    Plug 'jreybert/vimagit'
  " }
  " Visuals/syntax {
    Plug 'thaerkh/vim-indentguides'
    Plug 'flazz/vim-colorschemes'
    Plug 'luochen1990/rainbow'                  " bracket coloring
    Plug 'sheerun/vim-polyglot'                 " Syntax highlighting
    Plug 'blueyed/vim-diminactive'
    Plug 'stevearc/dressing.nvim'               " prettier boxes like on rename
  " }
  " Quality of Life {

    Plug 'nvim-telescope/telescope-file-browser.nvim' " telescope file browser
    Plug 'jiangmiao/auto-pairs'                 " autoclose deimiters
    Plug 'alvan/vim-closetag'                   " autoclose html esq tags
    Plug 'tpope/vim-surround'                   " edit tags/delimiters in pairs
    Plug 'mhinz/vim-startify'                   " start screen
    Plug 'ryanoasis/vim-devicons'               " fancy icons
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'ConradIrwin/vim-bracketed-paste'      " autoset paste mode
    Plug 'easymotion/vim-easymotion'            " better movement
    Plug 'tpope/vim-repeat'                     " repeat commands with .
    if has("vim-8.2.1978") || has("nvim")
      " Plug 'psliwka/vim-smoothie'
    endif
    if has("nvim")
      Plug 'antoinemadec/FixCursorHold.nvim'
      Plug 'sudormrfbin/cheatsheet.nvim'        " fuzzy cheatsheet
      Plug 'axieax/urlview.nvim'
      " run code snippets
      Plug 'michaelb/sniprun', {'do': 'bash install.sh'}
    endif
  " }
  " Multiline editing {
    " multicursor
    Plug 'mg979/vim-visual-multi', {'branch': 'master'}
    Plug 'matze/vim-move'                       " move blocks
    Plug 'tommcdo/vim-lion'                     " line up text
  " }
  " Tmux {
    Plug 'christoomey/vim-tmux-navigator'
    Plug 'tmux-plugins/vim-tmux'
  " }
  " snippets {
   Plug 'SirVer/ultisnips'
   Plug 'honza/vim-snippets'
  " }
  " markdown {
    Plug 'vimwiki/vimwiki'
    Plug 'gabrielelana/vim-markdown'            " syntax
    if has("nvim")
      " view markdown in a file
      Plug 'ellisonleao/glow.nvim', {'branch': 'main'}
  endif
  " }
call plug#end()


" ---- Plugin Settings ---------------------------------------------------------




autocmd VimEnter * CHADopen
autocmd VimEnter * TagbarToggle
autocmd StdinReadPre * let s:std_in=1






"  airline {
  " stops other insert modes from showing
  let g:airline_mode_map = {}
  let g:airline_mode_map['ic'] = 'INSERT'
  " display all buffers if one tab open
  let g:airline#extensions#tabline#enabled = 1
  let g:airline#extensions#tabline#tab_nr_type = 1  " buffer/tab number
  let g:airline#extensions#tabline#buffer_idx_mode = 1
  " how file paths are shown (they're not)
  let g:airline#extensions#tabline#fnamemod = ':t'
  let g:airline_powerline_fonts = 1             " powerline integration

  let g:airline_theme='bubblegum'
  "let g:airline_theme='onedark'

  let g:airline#extensions#tagbar#enabled = 1   " show where in file you are
  let g:airline#extensions#tagbar#flags = 'f'
  let g:airline#extensions#coc#enabled = 1
  let g:airline_detect_modified = 0             " stop color change of filepath
  let g:airline_section_c = airline#section#create(["%{expand('%:p:~:h:h:t')}/%{expand('%:p:h:t')}/%{expand('%:t')}"])
  if !exists('g:airline_symbols')
    let g:airline_symbols = {}
  endif
  let g:airline_symbols.modified = ''

" }
" NerdComment {
  let g:NERDSpaceDelims = 1                     " add space after comment char
  let g:NERDCompactSexyComs = 1                 " short syntax in comment blocks
" }
" Vim-Session {
  " let g:session_autosave = 'yes'                " auto save session periodically
" }
" Rainbow Brackets {
  let g:rainbow_active = 1                      " rainbow brackets
" }
" Indent Guides {
  let g:indentguides_spacechar = '|'            " indent guide chars
  let g:indentguides_tabchar = '|'
" }
" Limelight {
  let g:limelight_conceal_ctermfg = 'gray'      " Color dimming
  let g:limelight_conceal_guifg = 'DarkGray'
" }
" ctrlp {
  " ignore files in git and gitignore
  let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
  let g:ctrlp_map = 'cp'
" }
" vim wiki {
  " set markdown syntax and filepath
  let g:vimwiki_list = [{'path': '~/vimwiki/',
                      \ 'syntax': 'markdown', 'ext': '.md'}]
" }
" vim markdown {
  let g:markdown_mapping_switch_status = '<Leader>s'
" }
" coq {
  let g:coq_settings = { 'keymap.recommended': v:false,
        \ 'display.pum.fast_close': v:false}
" }


" ---- Extra Settings ----------------------------------------------------------





" change cursor shape
let &t_SI = "\<Esc>[6 q"                        " insert mode, vertical bar
let &t_SR = "\<Esc>[4 q"                        " replace mode, underscore
let &t_EI = "\<Esc>[2 q"                        " normal mode, block

" Auto Formatting
command! -nargs=0 Format :call CocActionAsync('format')
" highlight symbol on press
autocmd CursorHold * silent call CocActionAsync('highlight')


" colorscheme
set background=dark
let g:gruvbox_number_column = 'bg1'
let g:gruvbox_contrast_dark = 'soft'
autocmd vimenter * ++nested colorscheme gruvbox
"autocmd vimenter * ++nested colorscheme kanagawa





" Auto delete trailing whitespace, save cursor position
augroup noWhitespace
  autocmd!
  autocmd BufWritePre * let currPos = getpos(".")
  autocmd BufWritePre * %s/\s\+$//e
  autocmd BufWritePre * %s/\n\+\%$//e
  autocmd BufWritePre *.[ch] %s/\%$/\r/e
  autocmd BufWritePre * cal cursor(currPos[1], currPos[2])
augroup END





" WSL yank support
" if system('uname -r') =~ "microsoft"
  " augroup Yank
    " autocmd!
    " autocmd TextYankPost * :call system('/mnt/c/windows/system32/clip.exe ',@")
  " augroup END
" endif











" fixes lag of exiting insert/visual mode
if !has('gui_running')
  set ttimeoutlen=10
  augroup FastEscape
    autocmd!
    autocmd InsertEnter * set timeoutlen=0
    autocmd InsertLeave * set timeoutlen=1000
  augroup END
endif




" Override colors
augroup vimrc
  autocmd!
  autocmd ColorScheme * highlight clear Search
        \ | highlight Search ctermbg=None ctermfg=None guibg=Grey42
  autocmd ColorScheme * highlight CocHighlightText ctermbg=None ctermfg=None guibg=Grey35
augroup END




" remember fold
augroup remember_folds
  autocmd!
  autocmd BufWinLeave * silent! mkview
  autocmd BufWinEnter * silent! loadview
augroup END

" get rid of auto commenting
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o






" add cursorline when not in insert mode
augroup cursorLine
  autocmd!
  autocmd VimEnter * set cul
  autocmd InsertLeave * set cul
  autocmd InsertEnter * set nocul
augroup END




" show function signatures
augroup mygroup
  autocmd!
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')Lq
augroup END







" limelight integration
autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight!



" highlight text over 100 chars
if has("nvim")
  augroup highlightText
    autocmd!
    autocmd ColorScheme * highlight OverLength ctermbg=darkgrey guibg=Grey30
    autocmd ColorScheme * match OverLength /\%101v.*/
  augroup END
endif





"stop changing tab color when not saved, unless its not the selected tab
autocmd VimEnter *
   \ let g:airline#themes#bubblegum#palette.tabline = {
   \    'airline_tabmod':       ['#000000','#afd787',231,88,''],
   \    'airline_tabmod_unsel': ['#000000','#87afd7',231,52,''],
   \ } | :AirlineRefresh

" turn off suggestions in markdown only
autocmd FileType markdown let b:coc_suggest_disable = 1


" ---- Keybindings -------------------------------------------------------------

let mapleader = " "                               " Leader Character to Space
" COC {
  " errors
  nnoremap <leader> e <cmd>call coc#rpc#request('fillDiagnostics',
        \ [bufnr('%')])<CR><cmd>Trouble loclist<CR>
  nnoremap cl :silent! TroubleClose<CR>
  " nnoremap <silent> gn :lnext<CR>
  " nnoremap <silent> gp :lprev<CR>
  " Coc navigation
  nnoremap <silent> cy <Plug>(coc-type-definition)
  nnoremap <silent> ci <Plug>(coc-implementation)
  nnoremap <silent> cr <Plug>(coc-references)
  " better documentation and scroll through
  nnoremap <silent> D :call <SID>show_documentation()<CR>
  " inoremap <c-p> <C-\><C-O>:call CocActionAsync('showSignatureHelp')<cr>
  " if has('nvim-0.4.0') || has('patch-8.2.0750')
    " nnoremap <silent><nowait><expr> <A-j> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    " nnoremap <silent><nowait><expr> <A-k> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
    " inoremap <silent><nowait><expr> <A-j> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
    " inoremap <silent><nowait><expr> <A-k> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
    " vnoremap <silent><nowait><expr> <A-j> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    " vnoremap <silent><nowait><expr> <A-k> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  " endif
  " misc
  nnoremap <leader>ac   <Plug>(coc-codeaction-selected)w
  nnoremap <leader>qf  <Plug>(coc-fix-current)
  nnoremap <leader>rn <Plug>(coc-rename)
  nnoremap <silent> <leader>cd :CocDisable<cr>
  nnoremap <silent> <leader>ce :CocEnable<cr>
" }
" autocomplete {
  inoremap <expr> <CR>            pumvisible() ? "\<C-y>" : "\<CR>"
  inoremap <silent><expr> <Esc>   pumvisible() ? "\<C-e><Esc>" : "\<Esc>"
  inoremap <silent><expr> <C-c>   pumvisible() ? "\<C-e><C-c>" : "\<C-c>"
  inoremap <silent><expr> <CR>    pumvisible() ? (complete_info().selected == -1 ? "\<C-e><CR>" : "\<C-y>") : "\<CR>"
  inoremap <silent><expr> <BS>    pumvisible() ? "\<C-e><BS><C-e>"  : "\<BS>"
  inoremap <silent><expr> <tab>   pumvisible() ? "\<Down>" : "\<Tab>"
  inoremap <silent><expr> <Esc>[z pumvisible() ? "\<Up>" : "\<Esc>[z"
" }
" Multiline {
  let g:VM_maps = {}
  let g:VM_maps["Add Cursor Down"] = '<Leader>.'
  let g:VM_maps["Add Cursor Up"] = '<Leader>,'
  let g:VM_maps["Undo"] = 'u'
  let g:VM_maps["Redo"] = '<C-r>'
" }
" Goyo {
  nnoremap <silent> gy :Goyo<CR>
  nnoremap <silent> gy! :Goyo!<CR>
" }

" Limelight {
  nnoremap <silent> lime :Limelight<CR>
  nnoremap <silent> !lime :Limelight!<CR>
" }
" CHADTree {
  nnoremap <silent> <Leader>nt :CHADopen<CR>
  nnoremap <silent> <Leader>ng :CHADopen --version-ctl<CR>
" }
" Vim Session {
  nnoremap <Leader>o :OpenSession<CR><cr>
  nnoremap <Leader>s :SaveSession<CR>
" }
" UltiSnips {
  let g:UltiSnipsExpandTrigger="<s-tab>"
  let g:UltiSnipsJumpForwardTrigger="<c-b>"
  let g:UltiSnipsJumpBackwardTrigger="<c-z>"
" }
" Windows {
  " shift+key to switch windows
  nnoremap <S-h> <C-w>h
  nnoremap <S-j> <C-w>j
  nnoremap <S-k> <C-w>k
  nnoremap <S-l> <C-w>l
  " create new windows
  nnoremap <Leader>v <C-w>v
  nnoremap <Leader>h <C-w>s
" }



" Misc {
  " enter to get rid of highlights
  nnoremap m :noh<CR><CR>
  " exit vim command mode
  tnoremap <Esc> <C-\><C-n>
" }
" Delimiter Tabs {
  " toggle hl search
  nnoremap / :set hlsearch<cr>/
  nnoremap i :set nohlsearch<cr>i
  " ctrl+d out of delimiters
  inoremap <c-d> <left><c-o>/["';)>}\]]<cr><c-o>:noh<cr><right>
" }
" Visual Block Mode {
  command! Vb normal! <C-v>
" }
" renaming {
  " local
  nnoremap gr gd[{V%::s/<C-R>///gc<left><left><left>
  " global
  nnoremap gR gD:%s/<C-R>///gc<left><left><left>
" }
" auto save {
  nnoremap <c-s> :call ToggleAutoSave()<cr>
" }
" movement {
  inoremap <c-b> <c-o>0
  inoremap <c-a> <c-o>A
  vnoremap <c-b> ^
  vnoremap <c-a> $
" }


" ---- Functions --------------------------------------------------------------

function! s:show_documentation()
  if (index(['vim', 'help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" cycle between hunks
function! GitGutterNextHunkCycle()
  let line = line('.')
  silent! GitGutterNextHunk
  if line('.') == line
    1
    GitGutterNextHunk
  endif
endfunction

function! GitGutterPrevHunkCycle()
  let line = line('.')
  silent! GitGutterPrevHunk
  if line('.') == line
    normal! G
    GitGutterPrevHunk
  endif
endfunction

function! NextHunkAllBuffers()
  let line = line('.')
  GitGutterNextHunk
  if line('.') != line
    return
  endif
  let bufnr = bufnr('')
  while 1
    bnext
    if bufnr('') == bufnr
      return
    endif
    if !empty(GitGutterGetHunks())
      1
      GitGutterNextHunk
      return
    endif
  endwhile
endfunction

function! PrevHunkAllBuffers()
  let line = line('.')
  GitGutterPrevHunk
  if line('.') != line
    return
  endif
  let bufnr = bufnr('')
  while 1
    bprevious
    if bufnr('') == bufnr
      return
    endif
    if !empty(GitGutterGetHunks())
      normal! G
      GitGutterPrevHunk
      return
    endif
  endwhile
endfunction

" start with auto save on
augroup AutoSave
  autocmd CursorHoldI <buffer>
    \ if &readonly == 0 && filereadable(bufname('%')) | silent write | endif
augroup END

" auto save
function! ToggleAutoSave()
  " Switch the toggle variable
  let g:AutoSaveToggle = !get(g:, 'AutoSaveToggle', 1)
  " Reset group
  augroup AutoSave
    autocmd!
    echo "auto save off"
    let g:airline_detect_modified = 1
    let g:airline_symbols.modified = '+'
  augroup END
  " Enable if toggled on
  if g:AutoSaveToggle
    augroup AutoSave
      autocmd CursorHoldI <buffer>
        \ if &readonly == 0 && filereadable(bufname('%')) | silent write | endif
      echo "auto save on"
      let g:airline_detect_modified = 0
      let g:airline_symbols.modified = ''
    augroup END
  endif
endfunction


" ---- NVIM Specifics ---------------------------------------------------------

if has("nvim")
lua << EOF
-- this is space for changing the settings of nvim only plugins
EOF
endif









"------- Customs..



" telescope {
  nnoremap ff :Telescope find_files<cr>
  nnoremap fg :Telescope live_grep<cr>
  nnoremap fb :Telescope buffers<cr>
  nnoremap fh :Telescope help_tags<cr>
" }
" gitgutter {
  nnoremap diff :GitGutterDiffOrig<cr>
  " cycle continuously through single files
  nnoremap <silent> <Leader>nh :call GitGutterNextHunkCycle()<CR>
  nnoremap <silent> <Leader>ph :call GitGutterPrevHunkCycle()<CR>
  " cycle through multiple files
  nnoremap <silent> ]c :call NextHunkAllBuffers()<CR>
  nnoremap <silent> [c :call PrevHunkAllBuffers()<CR>
" }

" Buffers {
  " change buffers
  nnoremap <silent>    <A-]>  :w \| bn<cr>
  nnoremap <silent>    <A-[> :w \| bp<cr>
  nnoremap <Leader>bk :bd<cr>
" }
" Tagbar {
  nnoremap <Leader>tt :TagbarToggle<CR>
" }


"---vim start







"compiling shortcuts..
autocmd FileType cpp nmap <buffer> <F2> :sv  <bar>terminal bash -c "g++ -o %:r % && ./%:r && rm %:r"<CR>
autocmd FileType c nmap <buffer> <F2> :sv <bar>terminal bash -c "gcc -o %:r % -lncurses && ./%:r && rm %:r"<CR>
autocmd FileType cpp nmap <buffer> <F1>  :!bash -c "g++ -o %:r %"<CR>
autocmd FileType c nmap <buffer> <F1> :!bash -c "gcc -o %:r % "<CR>
autocmd Filetype cpp nmap <buffer> <F3> :!g++ -o %:r % -g<CR>
autocmd Filetype cpp nmap <buffer> <F4> :sv <bar>term gdb %:r<CR>
autocmd FileType cs nnoremap <buffer> <F2> :sv  <bar>:terminal cd %:p:h && dotnet run<CR>
au FileType cpp let b:comment_style="block"
au FileType cpp let b:comment_opener="/*"
au FileType cpp let b:comment_closer="*/"
au FileType cs let b:comment_style="block"
au FileType cs let b:comment_opener="/*"
au FileType cs let b:comment_closer="*/"
nnoremap <C-t> :sv <bar> terminal <CR>
nmap <F5> :CHADopen<CR>
nmap <F6> :TagbarToggle<CR>



nnoremap <leader>wv :vs<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>D :Explore<CR>
nnoremap <leader>q :qa<CR>






"telescope


nnoremap <Leader>pp <cmd>lua require'telescope.builtin'.builtin{}<CR>
nnoremap <Leader>fr <cmd>lua require'telescope.builtin'.oldfiles{}<CR>
nnoremap <Leader>, <cmd>lua require'telescope.builtin'.buffers{}<CR>
nnoremap <Leader>/ <cmd>lua require'telescope.builtin'.current_buffer_fuzzy_find{}<CR>
nnoremap <Leader>' <cmd>lua require'telescope.builtin'.marks{}<CR>
nnoremap <Leader>gf <cmd>lua require'telescope.builtin'.git_files{}<CR>
nnoremap <Leader>sf <cmd>lua require'telescope.builtin'.find_files{}<CR>
nnoremap <Leader>lg <cmd>lua require'telescope.builtin'.live_grep{}<CR>
nnoremap <Leader>cs <cmd>lua require'telescope.builtin'.colorscheme{}<CR>
