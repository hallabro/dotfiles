call plug#begin()

Plug 'gmarik/vundle'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'chriskempson/base16-vim'
Plug 'kien/ctrlp.vim'
Plug 'mileszs/ack.vim'
Plug 'danro/rename.vim'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-surround'
Plug 'Lokaltog/vim-easymotion'
Plug 'tpope/vim-sleuth'
Plug 'StanAngeloff/php.vim'
Plug 'SuperSimen/vim-twig'
Plug 'airblade/vim-gitgutter'

call plug#end()

" syntax and colors
colorscheme base16-macintosh
let base16colorspace=256
filetype plugin indent on
syntax on

" loading and saving
setglobal fileencoding=utf-8

" visual
set relativenumber
set number
set scrolloff=8
set shortmess+=I
set wildmenu
set linebreak
set cursorline
set lazyredraw

let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0

autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=18 guibg=#282828
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=19 guibg=#383838

" buffers
set hidden

" searching
set incsearch
set ignorecase
set smartcase

let g:ctrlp_clear_cache_on_exit = 0
let g:netrw_liststyle=3
let g:netrw_bufsettings = 'noma nomod nu nobl nowrap ro'

let &colorcolumn="120"

if executable("ag")
	let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
	let g:ag_highlight=1
	let g:ackprg = 'ag --vimgrep --smart-case'
endif

" keyboard
set backspace=2
let mapleader = "\<Space>"
let g:EasyMotion_do_mapping = 0
" let g:EasyMotion_keys='aoeuhtnsid,.pgcrkb' -- dvorak

map <Leader>s :w<CR>
imap <F8> _<Esc>mzi<S-Right><C-o>b<C-o>g~iw<C-o>`z<Del>
imap jk <Esc>

" tabs and spaces
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smarttab
set smartindent
set expandtab

" autocompletion and history
set history=300
set completeopt=longest,menuone

" sessions and backups
set backup
set undofile
set backupdir=$HOME/.local/share/nvim/backup
set directory=$HOME/.local/share/nvim/swap
set viewdir=$HOME/.local/share/nvim/view
set undodir=$HOME/.local/share/nvim/undo

" copu/paste
set clipboard=unnamedplus

" prefix deletion to black hole with <leader>
map <Leader>d "_d
map <Leader>D "_D

" recall last position in newly opened file
au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
	\| exe "normal! g'\"" | endif

cabbr <expr> %% expand('%:p:h')

" search/navigation
nnoremap <Leader>a :Ack! 
nnoremap <Leader>A :Ack! <cword><CR>
nmap <Leader><Space> <Plug>(easymotion-s2)
map <Leader> <Plug>(easymotion-prefix)

map <Tab> :CtrlP<CR>
map <S-Tab> :CtrlPMRUFiles<CR>
map <Leader><Tab> :Explore<CR>

map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
map <Leader>7 <Plug>(easymotion-sn)
map <Leader>w <Plug>(easymotion-w)
