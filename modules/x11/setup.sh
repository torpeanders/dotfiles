#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ln -fs ${DIR}/.Xresources ~
ln -fs ${DIR}/.xsessionrc ~
