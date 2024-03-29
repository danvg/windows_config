export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export TERM=xterm-256color
export EDITOR="nvim"
export VISUAL="nvim"

export CC="gcc"
export CXX="g++"
export CXXFLAGS="-std=c++11 -Wall -Wextra -pedantic"

export HISTFILE="$HOME/.cache/bash_history"
export HISTSIZE=10000
export SAVEHIST=$HISTSIZE
export HISTCONTROL=$HISTCONTROL:ignorespace:ignoredups
shopt -s histappend
shopt -s cmdhist

export LESSHISTFILE=-

alias ls="ls -hF --color=tty"
alias cls="clear"
alias rm="rm -vI"
alias cp="cp -iv"
alias mv="mv -iv"
alias ls="ls --color --ignore 'ntuser*' --ignore 'NTUSER*' --ignore '\$RECYCLE.BIN'"
alias cls="clear"

PROMPT_COMMAND='if [ $? = 0 ]; then\
  PS1="\[\e[34m\][\w]\[\e[0m\] \[\e[32m\]\[\e[0m\] "; else\
  PS1="\[\e[34m\][\w]\[\e[0m\] \[\e[31m\]\[\e[0m\] "; fi'
