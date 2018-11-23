#!/bin/bash

set -e

# Check os
case "$(uname -a)" in
    *Ubuntu*)   OS="ubuntu";;
    Darwin*)    OS="osx";;
    *)          echo "unknown os"; exit 1;;
esac

MODULES=($OS os-common bin emacs mpv tmux vim zsh)
if [ "$OS" != "osx" ]; then
    MODULES+=(i3 x11)
fi

# Install prerequisite packages
if [ "$OS" == "ubuntu" ]; then
    for MODULE in "${MODULES[@]}"; do
        cat modules/$MODULE/packages.apt 2>/dev/null
    done | sort -u | sudo xargs apt install -y
else
    brew update
    for MODULE in "${MODULES[@]}"; do
        cat modules/$MODULE/packages.brew 2>/dev/null
    done | sort -u | brew install
fi

# Run module setup scripts
for MODULE in "${MODULES[@]}"; do
    modules/$MODULE/setup.sh
done
