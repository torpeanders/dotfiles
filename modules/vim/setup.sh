#!/bin/bash
DIR="$( cd "$( dirname "$BASH_SOURCE" )" && pwd )"

ln -fs $DIR/.vimrc ~
ln -fs $DIR/.vim ~

VUNDLE_DIR=$DIR/.vim/bundle/Vundle.vim
if [ ! -e $VUNDLE_DIR ]; then
    git clone https://github.com/VundleVim/Vundle.vim.git $VUNDLE_DIR
fi
vim -c PluginInstall -c qa!
