# Cyril's dotfiles

## Clone .dotfiles in your home directory
    cd ~
    git clone https://github.com/ccressent/dotfiles .dotfiles

## Install Vundle
    mkdir -p .dotfiles/vim/bundle
    git clone https://github.com/gmarik/vundle .dotfiles/vim/bundle/vundle

## Create symbolic links as needed
    ln -s ~/.dotfiles/vim ~/.vim
    ln -s ~/.dotfiles/vimrc ~/.vimrc

## Make Vundle install all the vim plugins
    vim +PluginInstall +qall
