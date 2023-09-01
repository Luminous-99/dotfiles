
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

#Bash Prompt
PS1='\[\n\] ${debian_chroot:+($debian_chroot)}\[\e[0;32m╭╴\]\[\e[0;32m{\]\[\e[1;32m \u@\h\] \[\e[0;39m|\] \[\e[1;36m\d \t \e[0;32m}\]\] \]\e[m \[\n\] \[\e[0;32m╰╴(\[\e[1;36m\W\]\]\]\]\[\e[0;32m)\$ \]\e[m'

