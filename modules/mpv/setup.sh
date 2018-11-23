#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p ~/.config/mpv
ln -fs ${DIR}/config ~/.config/mpv/
ln -fs ${DIR}/input.conf ~/.config/mpv/
