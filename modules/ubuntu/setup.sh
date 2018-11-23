#!/bin/bash

if ! which rg 2>&1 > /dev/null; then
    echo "installing ripgrep..."
    curl -ssf https://static.rust-lang.org/rustup.sh | sh
    cargo install ripgrep
fi
