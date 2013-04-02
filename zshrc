#!/bin/zsh

export TZ=Europe/Dublin
export PATH=$PATH:/$HOME/bin
export PAGER='less'
export EDITOR='vim'

export http_proxy=http://proxy.ir.intel.com:911
export ftp_proxy=$http_proxy

setopt autocd

unsetopt BG_NICE					# I am not nice...

# prompt
autoload -U promptinit && promptinit
prompt redhat

## completion
autoload -U compinit && compinit

# Enable caching
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-patch ~/.zsh/cache

# Process names and PID completion
zstyle ':completion:*:*:*:*:processes' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always

# No clue what these lines do, but they work great
zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX + $#SUFFIX) / 3 )) )'
zstyle ':completion:*:descriptions' format "- %d -"
zstyle ':completion:*:corrections' format "- %d - (errors %e})"
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*' menu select
zstyle ':completion:*' verbose yes

# Complete commands that support --help
# compdef _gnu_generic progname

# On some systems the arrow keys do not work
# properly. These key bindings seem to solve 
# the problem.
# Taken from the ZSH FAQ.
bindkey '\e[A'  up-line-or-history
bindkey '\e[B'  down-line-or-history
bindkey '\e[C'  forward-char
bindkey '\e[D'  backward-char
bindkey '\eOA'  up-line-or-history
bindkey '\eOB'  down-line-or-history
bindkey '\eOC'  forward-char
bindkey '\eOD'  backward-char

# Set the screen window or xterm title
function title {
	if [[ $TERM == "screen" ]]; then
		print -nR $'\033k'$1$'\033'\\
		print -nR $'\033]0;'$2$'\a'
	elif [[ $TERM == "xtern" || $TERM == "rxvt" ]]; then
		print -nR $'\033]0;'$*$'\a'
	fi
}

function precmd {
	title zsh "$PWD"
}

function preexec {
	emulate -L zsh
	local -a cmd; cmd=(${(z)1})
	title $cmd[1]:t "$cmd[2,-1]"
}

