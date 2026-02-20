#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s histappend
HISTCONTROL=ignoreboth
HISTSIZE=2000
HISTFILESIZE=4000

shopt -s checkwinsize
alias ls='ls --color=auto'
alias grep='grep --color=auto'

time="\[\e[1;36m\]\d \t"
name="\[\e[1;33m\]\u@\h"
workspace="\[\e[1;36m\]\w"
new_line="\n"
upper_container="$workspace"

export PROMPT_DIRTRIM=3
export PS1=" $workspace \[\e[0;36m\]λ \[\e[00m\]"

function mkcd() { 
    if [ -z "$1" ]; then
        echo "Error: input is empty"
        return 1
    fi
    mkdir -p "$1" && cd "$1"
}

name=$(whoami)
name_size=${#name}

function pad_half() {
    padding=""
    max_pad=$(($1 / 2))
    for ((i = 0;i < $max_pad;i++)) do 
       padding="$padding " 
    done

    echo "$padding$2"
}
text_color="\e[$((0 + $RANDOM % 3));3$((1 + $RANDOM % 9))m"

padded_emoticon=$(pad_half $name_size "( '-')")
echo -e "\n\t$text_color Welcome, $name!\n\t$text_color$padded_emoticon\e\n\e[0m" 

export GTK_USE_PORTAL=1
export MOZ_ENABE_WAYLAND=1
export TERM=alacritty
export VISUAL=emacsclient
export EDITOR=emacsclient

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias la='ls -Ah'
alias ll='ls -ahlF'
alias l='ls -CFh'

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env

source "$HOME/.env"

function fman () {
    man -k "" \
	| fzf -e --preview="awk '{print $1}' <(echo {}) | xargs -r man" \
	| awk '{print $1}' \
	| xargs -r man
}

[ -f "/bin/auto-cpufreq" ] && eval "$(_AUTO_CPUFREQ_COMPLETE=bash_source auto-cpufreq)"
