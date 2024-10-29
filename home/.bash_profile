#!/bin/bash

if [ -n "$BASH_VERSION" ] && [ -f $HOME/.bashrc ];then
    source $HOME/.bashrc
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export PYTHON_ENV="$HOME/.local/share/py"
export PATH="$PATH:~/.config/doom/bin"
export SBCL_HOME="/usr/lib/sbcl/"
source "$HOME/.env"
