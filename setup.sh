#!/bin/bash

# Check os
case "$(uname -a)" in
    *Ubuntu*)   OS="ubuntu";;
    Darwin*)    OS="osx";;
    *)          echo "unknown os"; exit 1;;
esac

MODULES=($OS os-common bin emacs mpv tmux vim zsh)
if dpkg -l ubuntu-desktop > /dev/null 2>&1; then
    MODULES+=(i3 x11)
fi

# Install prerequisite packages
if [ "$OS" == "ubuntu" ]; then
    PKG=($(for MODULE in "${MODULES[@]}"; do
               cat modules/$MODULE/packages.apt 2>/dev/null
           done))
    if [ -n "${PKG[*]}" ]; then
        sudo apt install -y "${PKG[@]}"
    fi
else
    PKG=($(for MODULE in "${MODULES[@]}"; do
               cat modules/$MODULE/packages.brew 2>/dev/null
           done))
    if [ -n "${PKG[*]}" ]; then
        brew update
        brew install "${PKG[@]}"
    fi
fi

# Run module setup scripts
for MODULE in "${MODULES[@]}"; do
    modules/$MODULE/setup.sh
done
