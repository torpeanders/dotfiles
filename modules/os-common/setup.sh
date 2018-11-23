#!/bin/bash

if ! which fasd 2>&1 > /dev/null; then
    echo "installing fasd..."
    rm -rf /tmp/fasd
    git clone https://github.com/clvv/fasd.git /tmp/fasd
    cd /tmp/fasd
    sudo make install
fi

if ! which fzf 2>&1 > /dev/null; then
    echo "installing fzf..."
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
    ~/.fzf/install
fi
