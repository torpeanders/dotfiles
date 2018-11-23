#!/bin/bash

if ! which brew 2>&1 > /dev/null; then
    echo "installing homebrew..."
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
