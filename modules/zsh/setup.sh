#!/bin/bash
DIR="$( cd "$( dirname "$BASH_SOURCE" )" && pwd )"

ln -fs $DIR/.zshrc ~

case "$(uname -a)" in
    *Ubuntu*)
        if [ ! "$SHELL" -ef "$(which zsh)" ]; then
            echo "Change the SHELL to $(which zsh). You'll be prompted for your password..."
            chsh -s $(which zsh)
        fi
    ;;
    *) : ;;
esac
