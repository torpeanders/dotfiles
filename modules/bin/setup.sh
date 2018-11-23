#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p ~/bin
ln -fs ${DIR}/fuzzy_lock.sh ~/bin/
ln -fs ${DIR}/osx ~/bin/
ln -fs ${DIR}/socky ~/bin/
ln -fs ${DIR}/ssh-wrapper ~/bin/
ln -fs ${DIR}/start_session ~/bin/
ln -fs ${DIR}/xsetup.sh ~/bin/
ln -fs ${DIR}/zshsetup ~/bin/
