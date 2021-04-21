export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export TERM=xterm-256color
export EDITOR="nvim"
export VISUAL="nvim"

export CC="gcc"
export CXX="g++"
export CXXFLAGS="-std=c++11 -Wall -Wextra -pedantic"

HISTFILE="~/.cache/zhistory"
HISTSIZE=10000
SAVEHIST=$HISTSIZE

setopt correct
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history
unsetopt BEEP

bindkey -v
zstyle :compinstall filename "~/.zshrc"
autoload -Uz compinit
compinit -d "~/.cache/zcompdump"

alias cls="clear"
alias rm="rm -vI"
alias cp="cp -iv"
alias mv="mv -iv"
alias ls="ls --color --ignore 'ntuser*' --ignore 'NTUSER*' --ignore '\$RECYCLE.BIN'"
alias cls="clear"

source ~/.zsh_plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source ~/.zsh_plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.zsh_plugins/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh
