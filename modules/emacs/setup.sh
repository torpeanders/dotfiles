#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SPC="$HOME/.emacs.d"

# Spacemacs setup
if [ ! -d "$SPC" ]; then
    echo "installing spacemacs..."
    git clone git@github.com:syl20bnr/spacemacs.git "$SPC" -b develop
fi

ln -fs "${DIR}"/.spacemacs "$HOME"/

# Spacemacs:
ln -fs "$DIR"/{defuns,config,site-lisp} "$HOME"/.emacs.d/
