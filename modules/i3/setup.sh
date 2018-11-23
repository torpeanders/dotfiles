#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p ~/.config/
ln -fs ${DIR}/i3 ~/.config/
ln -fs ${DIR}/i3blocks ~/.config/
