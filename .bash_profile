#!/bin/bash

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

name="\[\e[1;32m\]\u@\h"
time="\[\e[1;36m\]\d \t"
workspace="\[\e[1;36m\]\W"
new_line="\[\n\]"
upper_container="\[\e[0;32m\]{ $name \[\e[0;39m\]| $time \e[0;32m\]}"

#Bash Prompt

#original
#PS1="\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$"


PS1="$new_line \[\e[0;32m\]╭╴$upper_container ${debian_chroot:+($debian_chroot)} $new_line \[\e[0;32m\]╰╴($workspace\[\e[0;32m)\]\$ \[\e[0m\]"
