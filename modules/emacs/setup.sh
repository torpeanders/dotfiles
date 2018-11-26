#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SPC=$DIR/.spacemacs.d
if [ ! -e $SPC ]; then
    git clone git@github.com:syl20bnr/spacemacs.git "$SPC" -b develop
fi

ln -fs $SPC ~/.emacs.d

ln -fs "${DIR}"/.spacemacs "$HOME"/
ln -fs "$DIR"/{defuns,config,site-lisp} "$HOME"/.emacs.d/
