call plug#begin(stdpath('data') . 'vimplug')
    Plug 'nvim-lua/plenary.nvim'
Plug 'wakatime/vim-wakatime'

    Plug 'breuckelen/vim-resize'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'tranvansang/octave.vim'
    Plug 'Shougo/unite.vim'
    Plug 'puremourning/vimspector'
    Plug 'godlygeek/tabular'
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'szw/vim-maximizer'
    Plug 'Shougo/vimfiler.vim'
    Plug 'hrsh7th/nvim-compe'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'kyazdani42/nvim-web-devicons' " for file icons
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'lervag/vimtex'
    Plug 'nikvdp/neomux'
    Plug 'tpope/vim-ragtag'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-unimpaired'
    Plug 'tpope/vim-eunuch'
    Plug 'tpope/vim-fugitive'
    Plug 'tomtom/tcomment_vim'
    Plug 'machakann/vim-highlightedyank'  " Highlight yanks
    Plug 'folke/lsp-colors.nvim'
    Plug 'neovim/nvim-lspconfig'
    Plug 'williamboman/nvim-lsp-installer'
    Plug 'chriskempson/base16-vim'
    Plug 'morhetz/gruvbox'
    Plug 'junegunn/fzf'
    Plug 'ahmedkhalf/project.nvim'
    Plug 'jiangmiao/auto-pairs'
    Plug 'mhinz/vim-startify'
    Plug 'voldikss/vim-floaterm'
    Plug 'vim-airline/vim-airline'
    Plug 'axvr/org.vim'
    Plug 'jceb/vim-orgmode'
    Plug 'ptzz/lf.vim'
    Plug 'dhruvasagar/vim-table-mode'
    Plug 'jessedhillon/vim-easycomment'
    Plug 'chrisbra/csv.vim'
    Plug 'dhruvasagar/vim-table-mode'
    Plug 'kabouzeid/nvim-jellybeans'
    Plug 'SirVer/ultisnips'
    Plug 'honza/vim-snippets'
    Plug 'preservim/tagbar'
    Plug 'ap/vim-css-color'
    Plug 'axvr/photon.vim'
    Plug 'dikiaap/minimalist'
    Plug 'tomasiser/vim-code-dark'
    Plug 'AlessandroYorba/Alduin'
    Plug 'vim-pandoc/vim-markdownfootnotes'
    Plug 'rktjmp/lush.nvim'
    Plug 'kyazdani42/nvim-web-devicons'
call plug#end()



autocmd VimEnter * VimFilerExplorer

" basic settings
syntax on
set number
set relativenumber
set ignorecase      " ignore case
set smartcase     " but don't ignore it, when search string contains uppercase letters
set nocompatible
set incsearch        " do incremental searching
set ruler
set smartindent
set shiftwidth=4
set hlsearch
set backspace=indent,eol,start " allow backspacing over everything in insert mode
set autoindent
set mouse=a  " mouse support
let g:NERDCreateDefaultMappings = 0
let g:NERDSpaceDelims = 1
xnoremap <Leader>ci <cmd>call NERDComment('n', 'toggle')<CR>
nnoremap <Leader>ci <cmd>call NERDComment('n', 'toggle')<CR>






lua <<EOF
require("treesitter")
require("completion")
require('telescope').load_extension('projects')
require("project_nvim").setup {
  manual_mode = false,
  detection_methods = { "lsp", "pattern" },
  patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json" },
  ignore_lsp = {},
  exclude_dirs = {},
  show_hidden = false,
  silent_chdir = true,
  datapath = vim.fn.stdpath("data"),
}
vim.api.nvim_set_keymap("i", "<CR>", "compe#confirm({ 'keys': '<CR>', 'select': v:true })", { expr = true })
EOF










"LSP severs configs..

lua << EOF
--local pid = vim.fn.getpid()
--local omnisharp_bin = "/usr/bin/omnisharp"
--require'lspconfig'.omnisharp.setup{
--    cmd = { omnisharp_bin, "--languageserver" , "--hostPID", tostring(pid) };
--}
--require'lspconfig'.clangd.setup{}
--vim.cmd [[autocmd ColorScheme * highlight NormalFloat guibg=#1f2335]]
--vim.cmd [[autocmd ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]]
EOF







"
"compiling shortcuts..
autocmd FileType cpp nmap <buffer> <F2> :sv  <bar>terminal bash -c "g++ -o %:r % && ./%:r && rm %:r"<CR>
autocmd FileType c nmap <buffer> <F2> :sv <bar>terminal bash -c "gcc -o %:r % -lncurses && ./%:r && rm %:r"<CR>
autocmd FileType cpp nmap <buffer> <F1>  :!bash -c "g++ -o %:r %"<CR>
autocmd FileType c nmap <buffer> <F1> :!bash -c "gcc -o %:r % "<CR>
autocmd Filetype cpp nmap <buffer> <F3> :!g++ -o %:r % -g<CR>
autocmd Filetype cpp nmap <buffer> <F4> :sv <bar>term gdb %:r<CR>
autocmd FileType cs nnoremap <buffer> <F2> :sv  <bar>:terminal cd %:p:h && dotnet run<CR>


autocmd StdinReadPre * let s:std_in=1
autocmd FileType python,c,cpp TagbarOpen,cs
autocmd VimEnter * wincmd w
autocmd VimEnter * TagbarOpen







"sessions
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
set undodir=~/.vim/undodir
set undofile
set undolevels=1000
set undoreload=10000
tnoremap <silent> <C-[><C-[> <C-\><C-n>








" comments
vmap <silent> <C-c> :call ToggleCommentVisual()<CR>
nmap <silent> <C-c> :call ToggleCommentLine()<CR>
au FileType cpp let b:comment_style="block"
au FileType cpp let b:comment_opener="/*"
au FileType cpp let b:comment_closer="*/"
au FileType cs let b:comment_style="block"
au FileType cs let b:comment_opener="/*"
au FileType cs let b:comment_closer="*/"







"Colors
let g:airline_theme = 'codedark'
autocmd InsertLeave,WinEnter * set cursorline
autocmd InsertEnter,WinLeave * set nocursorline
set termguicolors
set termguicolors " this variable must be enabled for colors to be applied properly
highlight NvimTreeFolderIcon guibg=blue







"mapping
nnoremap <Leader>o o<Esc>
nnoremap <Leader>O O<Esc>
command! -nargs=* T split | terminal <args>
command! -nargs=* VT vsplit | terminal <args>
nnoremap <C-n> :NvimTreeToggle<CR>
nnoremap <C-s> :sv<CR>
nnoremap <C-x> :vs<CR>
nnoremap <C-t> :sv <bar> terminal <CR>
nmap <F5> :VimFilerExplorer<CR>
nmap <F6> :TagbarToggle<CR>
nmap <c-q> :qa <CR>




" >> Lsp key bindings
nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gD    <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gi    <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> gf    <cmd>lua vim.lsp.buf.formatting()<CR>



"nnoremap <silent> K     <cmd>Lspsaga hover_doc<CR>
"nnoremap <silent> <C-p> <cmd>Lspsaga diagnostic_jump_prev<CR>
"nnoremap <silent> <C-n> <cmd>Lspsaga diagnostic_jump_next<CR>
"nnoremap <silent> <C-n> <cmd>lua require'lspsaga.diagnostic'.lsp_jump_diagnostic_next()<CR>


"nnoremap <silent>gn <cmd>lua require('lspsaga.rename').rename()<CR>
"nnoremap <silent> ga    <cmd>Lspsaga code_action<CR>
"xnoremap <silent> ga    <cmd>Lspsaga range_code_action<CR>
"nnoremap <silent> gs    <cmd>Lspsaga signature_help<CR>
"nnoremap <silent><leader>ca <cmd>lua require('lspsaga.codeaction').code_action()<CR>
"vnoremap <silent><leader>ca :<C-U>lua require('lspsaga.codeaction').range_code_action()<CR>

"clipboard
set clipboard+=unnamedplus

" set leader key to ,
let g:mapleader=" "





"keys
nnoremap <leader>wv :vs<CR>
nnoremap <leader>ws :split<CR>
nnoremap <leader>. :VimFiler<CR>
nnoremap <leader>q :qa<CR>



"snip
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsEditSplit="vertical"


"telescope


nnoremap <Leader>pp <cmd>lua require'telescope.builtin'.builtin{}<CR>
nnoremap <Leader>fr <cmd>lua require'telescope.builtin'.oldfiles{}<CR>
nnoremap <Leader>, <cmd>lua require'telescope.builtin'.buffers{}<CR>
nnoremap <Leader>/ <cmd>lua require'telescope.builtin'.current_buffer_fuzzy_find{}<CR>
nnoremap <Leader>' <cmd>lua require'telescope.builtin'.marks{}<CR>
nnoremap <Leader>g <cmd>lua require'telescope.builtin'.git_files{}<CR>
nnoremap <Leader>sf <cmd>lua require'telescope.builtin'.find_files{}<CR>
nnoremap <Leader>lg <cmd>lua require'telescope.builtin'.live_grep{}<CR>
nnoremap <Leader>cs <cmd>lua require'telescope.builtin'.colorscheme{}<CR>




nnoremap <silent>    <A-[> :BufferPrevious<CR>
nnoremap <silent>    <A-]> :BufferNext<CR>
nnoremap <silent>    <A-1> :BufferGoto 1<CR>
nnoremap <silent>    <A-2> :BufferGoto 2<CR>
nnoremap <silent>    <A-3> :BufferGoto 3<CR>
nnoremap <silent>    <A-4> :BufferGoto 4<CR>
nnoremap <silent>    <A-5> :BufferGoto 5<CR>
nnoremap <silent>    <A-6> :BufferGoto 6<CR>
nnoremap <silent>    <A-7> :BufferGoto 7<CR>
nnoremap <silent>    <A-8> :BufferGoto 8<CR>
nnoremap <silent>    <A-9> :BufferLast<CR>
nnoremap <silent>    <A-c> :BufferClose<CR>


"" WinEnter wincmd
nmap <silent> <c-k> :wincmd k<CR>
nmap <silent> <c-j> :wincmd j<CR>
nmap <silent> <c-h> :wincmd h<CR>
nmap <silent> <c-l> :wincmd l<CR>




  ""debug
nnoremap <leader>dd :call vimspector#Launch() <CR>
nnoremap <leader>de :call vimspector#Reset()  <CR>
nnoremap <leader>dtcb :call vimspector#CleanLineBreakPoint()<CR>
nnoremap <leader>dbp :call vimspector#ToggleBreakpoint()<CR>

nmap <F7> <Plug>VimspectorStepInto
nmap <F8> <Plug>VimspectorStepOver
nmap <F9> <Plug>VimspectorStepOut
nmap <F10> <Plug>VimspectorRestart



inoremap <expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr> <S-TAB> pumvisible() ? "\<C-p>" : "\<TAB>"


autocmd VimEnter * source ~/.config/nvim/init.vim
colorscheme jellybeans
