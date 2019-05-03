# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.scripts" ] ; then
    export PATH="$HOME/.scripts:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    export PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes cargo binaries if they exist
if [ -d "$HOME/.cargo/bin" ] ; then
    export PATH="$HOME/.cargo/bin:$PATH"
fi

# Set the list of directories that Zsh searches for programs.
path=(
    /usr/local/{bin,sbin}
    $path
)

# Load antigen from somewhere
if [ -f /usr/local/share/antigen/antigen.zsh ]; then
    source /usr/local/share/antigen/antigen.zsh
elif [ -f /usr/share/zsh-antigen/antigen.zsh ]; then
    source /usr/share/zsh-antigen/antigen.zsh
else
    if [ ! -f $HOME/antigen.zsh ]; then
        curl -L git.io/antigen > $HOME/antigen.zsh
    fi
    source $HOME/antigen.zsh
fi

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle fasd
antigen bundle git
antigen bundle pip
antigen bundle python
antigen bundle command-not-found
antigen bundle common-aliases
antigen bundle https://github.com/denysdovhan/spaceship-prompt
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle mafredri/zsh-async

# Load the theme.
antigen theme denysdovhan/spaceship-prompt spaceship

SPACESHIP_DIR_COLOR="blue"
SPACESHIP_EXIT_CODE_PREFIX="["
SPACESHIP_EXIT_CODE_SUFFIX="]"
SPACESHIP_EXIT_CODE_SYMBOL=""
SPACESHIP_EXIT_CODE_SHOW="true"
SPACESHIP_TIME_SHOW="true"
SPACESHIP_GIT_STATUS_DIVERGED="$SPACESHIP_GIT_STATUS_AHEAD$SPACESHIP_GIT_STATUS_BEHIND"
#SPACESHIP_HOST_SHOW="false"
#SPACESHIP_USER_SHOW="false"

# OS specific plugins
if [[ $CURRENT_OS == 'OS X' ]]; then
    antigen bundle brew
    antigen bundle brew-cask
    antigen bundle osx
elif [[ $CURRENT_OS == 'Linux' ]]; then
    :
elif [[ $CURRENT_OS == 'Cygwin' ]]; then
    :
fi

# Tell Antigen that you're done.
antigen apply

# FASD
eval "$(fasd --init posix-alias zsh-hook)"

# FZF
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Environment
if [ -z "$USER" ]; then
    USER=$LOGNAME
fi

# Functions
fvim () {
    vim $(find -type f -and -not -path "*_build*" -name "$1*")
}

gvim () {
    vim -q <(git grep $1)
}

rgvim() {
    vim -q <(rg --line-number --column --no-heading --fixed-strings --ignore-case --no-ignore --hidden $1)
}

if type ssh-wrapper > /dev/null 2>&1; then
    ssh() {
        ssh-wrapper "$@"
    }
fi

# Aliases
alias a='fasd -a'        # any
alias s='fasd -si'       # show / search / select
alias d='fasd -d'        # directory
alias f='fasd -f'        # file
alias sd='fasd -sid'     # interactive directory selection
alias sf='fasd -sif'     # interactive file selection
alias z='fasd_cd -d'     # cd, same functionality as j in autojump
alias zz='fasd_cd -d -i' # cd with interactive selection

alias v='f -e vim'
alias sv='sf -e vim'
alias ec='f -e emacsclient'
alias ecd='d -e emacsclient'
alias sec='sf -e emacsclient'
alias secd='sd -e emacsclient'

alias rm='rm -i'

alias fuck='sudo $(fc -ln -1)'

# Misc zsh setup
zstyle ':completion:*' special-dirs true
zstyle ':completion:*:*' ignored-patterns '*ORIG_HEAD'

DISABLE_AUTO_TITLE="true"

unsetopt share_history

export EDITOR=vim
