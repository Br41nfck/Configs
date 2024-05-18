" Reload vimrc configuratuion file
set visualbell
set nolist
set guifont=Ubuntu\ Mono\ 14
set nocompatible
syntax enable
filetype on
filetype off
set autoindent
set autoread
set backspace=2
set clipboard=unnamed
set backupcopy=yes
set directory-=.
set encoding=utf-8
set expandtab
" Searching
nnoremap / /\v
vnoremap / /\v
set ignorecase
set smartcase
set showmatch
set hlsearch
set incsearch
set laststatus=2
set number
set ruler
set scrolloff=3
set shiftwidth=3
set softtabstop=2
set tabstop=8
set wildmenu
set hidden
set ttyfast
set showmode
set showcmd

" Plugins
call plug#begin()

Plug 'sheerun/vim-polyglot'

call plug#end()
