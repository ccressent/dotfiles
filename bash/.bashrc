HISTSIZE=100000
HISTFILESIZE=100000
HISTCONTROL=ignorespace:ignoredups:erasedups
HISTIGNORE='ls:bg:fg:history'
shopt -s histappend

PROMPT_COMMAND='history -a'

export PLATFORM=$(uname -s)

export PAGER=less

export GOPATH=$HOME/go
export PATH=$HOME/bin:$PATH:$GOPATH/bin

if command -v stack > /dev/null; then
    PATH=$PATH:`stack path --silent --local-bin`
fi

if command -v direnv > /dev/null; then
    eval "$(direnv hook bash)"
fi

if [ "$PLATFORM" = Linux ]; then
    alias ls='ls --color=auto'
fi

if [ "$PLATFORM" = Darwin ]; then
    alias ls='ls -G'
    alias vim='/Applications/MacVim.app/Contents/MacOS/Vim'

    [ -f $(brew --prefix)/etc/bash_completion ] && . $(brew --prefix)/etc/bash_completion

    # z.sh doesn't seem to play nice with prompt_jobs()...
    # [ -f $(brew --prefix)/etc/profile.d/z.sh ] && . $(brew --prefix)/etc/profile.d/z.sh
fi

alias vi='vim'
alias ll='ls -lhA'
alias untar='tar -zxvf'
alias pubip='curl https://ipinfo.io/ip'

export EDITOR='vim'

### Bash completion
[ -f /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

# gitdiffb
gitdiffb() {
    if [ $# -ne 2 ]; then
        echo 2 branch names required
        return
    fi
    git log --graph \
    --format="%C(red)%h%C(reset) -%C(yellow)%d%C(reset) %s %C(green)(%cr)%C(reset)" \
    --abbrev-commit --date=relative $1..$2
}

### fzf

# With Nix, the path to the relevant files to source is given by fzf-share.
# With other packages managers, we try the usual path.
if command -v fzf-share > /dev/null; then
    . $(fzf-share)/completion.bash
    . $(fzf-share)/key-bindings.bash
else
    [ -f /usr/share/fzf/completion.bash ] && . /usr/share/fzf/completion.bash
    [ -f /usr/share/fzf/key-bindings.bash ] && . /usr/share/fzf/key-bindings.bash
fi

export FZF_DEFAULT_OPTS='
    --color dark,hl:33,hl+:37,fg+:235,bg+:136,fg+:254
    --color info:254,prompt:37,spinner:108,pointer:235,marker:235
    --color 16
'

# fd - fuzzy find a directory to cd into
fd() {
    DIR=`find ${1:-*} -path '*/\.*' -prune -o -type d -print 2> /dev/null | fzf-tmux` \
        && cd "$DIR"
}

# flog - git log mini-browser with fzf
flog() {
    git log --graph --color=always \
        --format="%C(auto)%h%d %s %C(white)%cr" "$@" |
    fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
        --header "Press CTRL-S to toggle sort" \
        --preview "echo {} | grep -o '[a-f0-9]\{7\}' | head -1 |
                   xargs -I % sh -c 'git show --color=always % | head -200 '" \
        --bind "enter:execute:echo {} | grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R'"
}

# ftpane - switch tmux pane using fzf
# BROKEN
ftpane() {
  local panes current_window current_pane target target session target_window target_pane

  panes=$(tmux list-panes -a -F '#S:#W:#P - #{pane_current_path} #{pane_current_command}')
  current_pane=$(tmux display-message -p '#S:#I:#P')
  current_window=$(tmux display-message -p '#I')

  target=$(echo "$panes" | grep -v "$current_pane" | fzf +m --reverse) || return

  target_session=$(echo $target | awk 'BEGIN{FS=":|-"} {print $1}')
  target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}')
  target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$3}' | cut -c 1)

  if [[ $current_window -eq $target_window ]]; then
    tmux select-pane -t ${target_window}.${target_pane}
  else
    tmux select-pane -t ${target_session}:${target_window}.${target_pane} &&
    tmux select-window -t ${target_session}:${target_window} &&
    tmux switch-client -t ${target_session}
  fi
}

# fstash - git stash mini-browser with fzf
# fstash() {
# }

# docker-start - select a docker container to start
docker-start() {
    local cid
    cid=$(sudo docker ps -a | fzf --header-lines=1 -q "$1" | awk '{print $1}')

    [ -n "$cid" ] && sudo docker start "$cid"
}

# docker-stop - select a running docker container to stop
docker-stop() {
    local cid
    cid=$(sudo docker ps | fzf --header-lines=1 -q "$1" | awk '{print $1}')

    [ -n "$cid" ] && sudo docker stop "$cid"
}

prompt_jobscount() {
    local njobs=$(jobs -p | wc -l)
    ((njobs)) && printf '{%u}' "${njobs}"
}

prompt_off() {
    PS1='\$ '
}

function prompt_on() {
    PS1='\W$(prompt_jobscount)\$ '
}
prompt_on
