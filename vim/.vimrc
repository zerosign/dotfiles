autocmd! bufwritepost .vimrc source %

set number
set ruler
set showcmd
set matchtime=4
set nolist
set foldenable
set foldmethod=marker
set splitbelow
set splitright

set history=256
set autowrite
set autoread
set modeline

set wildmenu
set wildmode=list:longest

set nowritebackup
set nobackup
set hidden

set hlsearch
set smartcase
set incsearch

set wrap
" set textwidth=3
set tabstop=3
set softtabstop=3
set shiftwidth=3
set smarttab

" Vundle init
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'editorconfig/editorconfig-vim'
Plugin 'gmarik/Vundle.vim'

Plugin 'now/vim-man'

Plugin 'tComment'
nnoremap // :TComment<CR>
vnoremap // :TComment<CR>

nnoremap <C-S-PageUp> :tabnext<CR>
nnoremap <C-S-PageDown> :tabprev<CR>
nnoremap <C-Insert> :tabnew<CR>
nnoremap <C-Delete> :tabclose<CR>

call vundle#end()
filetype plugin indent on

syntax on
