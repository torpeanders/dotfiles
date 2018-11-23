#!/bin/bash
DIR="$( cd "$( dirname "$BASH_SOURCE" )" && pwd )"

ln -fs $DIR/.zshrc ~

if [ ! "$SHELL" -ef "$(which zsh)" ]; then
    echo "Gonna change the SHELL to $(which zsh). You'll be prompted for your password..."
    chsh -s $(which zsh)
fi
