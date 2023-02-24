set encoding=utf-8
set tabstop=2
set shiftwidth=2
set expandtab
set smarttab
set autoindent
set nowrap
set smartcase
set showmatch
set title
set ruler
set et
set number relativenumber
set incsearch
set hlsearch
set autoread
set autowrite
set nobackup
set nowritebackup
set noswapfile
set nocompatible
" title required for i3 status checking, modified indicator at end
"set titlestring=%t%(\ \(%F\)%)%a\ -\ VIM%(\ %M%)
set titlestring=%(%F%)%a\ -\ VIM%(\ %M%)
set t_Co=16
" undo cursorlinenr underline (was introduced Aug '19)
hi CursorLineNr cterm=bold

" allows buffers to be hidden if you modified a buffer
set hidden
filetype off
syntax on

" removes possibility to define function keys that start with <Esc>
" see if has implications
set noesckeys


