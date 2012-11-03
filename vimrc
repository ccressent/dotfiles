" General
set nocompatible		" Use vim default settings
set history=300			" Number of line of history vim should remember
set autoread			" Automatically reread a file if it has changed

filetype on				" Automatic filetype detection
filetype plugin on		" Enable filetype specific plugins
filetype indent on		" Enable filetype specific indentation

" Press space to clear search highlighting and any message already displayed
nnoremap <silent> <Space> :silent noh<Bar>echo<CR>


" Interface
set title				" Show title in the console title bar
set ruler				" Show the current position inside the file

set scrolloff=10		" Keep 10 lines when scrolling

set expandtab			" Expand all tab characters to spaces
set tabstop=4			" A tab character is 4 spaces
set shiftwidth=4		" Shift is 4 spaces

set showmatch			" Show matching brackets/parentheses

set incsearch
set ignorecase			" Ignore case when searching

set noerrorbells		" No sound on errors
set novisualbell		" No flash on errors
set t_vb=

set dictionary=/usr/share/dict/words


" Tags
set tags=./tags;


" Colors and fonts
syntax on				" Enable syntax highlighting
colorscheme desert


" File backups
set nobackup
set nowritebackup
set noswapfile


" Keymaps
map <F9> :make<CR>


if has("autocmd")
	autocmd FileType c,cpp,sh,pl set autoindent
	autocmd FileType c,cpp,sh,pl set smartindent
	autocmd FileType c,cpp,sh,pl set cindent
endif

