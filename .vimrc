set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'git@github.com:Valloric/YouCompleteMe.git'
Plugin 'git@github.com:altercation/vim-colors-solarized.git'
Plugin 'git@github.com:kien/ctrlp.vim.git'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ

inoremap jj <Esc>
inoremap JJ <Esc>
map <c-l> :bnext<CR>
map <c-k> :bprev<CR>
filetype plugin indent on
syntax enable
colorscheme solarized
set background=dark
set ttimeoutlen=50  "Reduce delay for key codes, especially <Esc>
set number
set history=1000
set hidden "Allow buffer switching without saving
set autoread "Auto reload if file saved externally
set backspace=indent,eol,start
set noswapfile
set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab
" Display tabs and trailing spaces visually
set list listchars=tab:\ \ ,trail:·
set shiftround
set linebreak
let &showbreak='↪ '
set nowrap       "Don't wrap lines
set linebreak    "Wrap lines at convenient points
set mouse=a "Enable mouse
set spell
set shortmess=aoOtI "Get rid of Press Enter
autocmd CursorHold,CursorHoldI,InsertLeave * silent! wall "Auto-save
set updatetime=1000
set completeopt-=preview
