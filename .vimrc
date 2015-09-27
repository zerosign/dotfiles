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

Plugin 'gmarik/Vundle.vim'

Plugin 'Rip-Rip/clang_complete'
Plugin 'davidhalter/jedi-vim'
Plugin 'nsf/gocode', {'rtp' : 'vim'}

Plugin 'now/vim-man'

Plugin 'mattn/emmet-vim'

Plugin 'tComment'
nnoremap // :TComment<CR>
vnoremap // :TComment<CR>

Plugin 'vim-pandoc/vim-pandoc'

nnoremap <C-S-PageUp> :tabnext<CR>
nnoremap <C-S-PageDown> :tabprev<CR>
nnoremap <C-Insert> :tabnew<CR>
nnoremap <C-Delete> :tabclose<CR>

"Plugin 'Shougo/vimproc.vim'
Plugin 'eagletmt/neco-ghc'

"Plugin for Ruby (rsense)
Plugin 'm2ym/rsense', {'rtp' : 'etc'}

call vundle#end()
filetype plugin indent on


syntax on

let g:rsenseHome = "/home/zerosign/Repositories/rsense"

au BufRead,BufNewFile *.hs,*.lhs setlocal filetype=haskell omnifunc=necoghc#omnifunc
