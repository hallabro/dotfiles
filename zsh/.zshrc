export PATH=$PATH:$HOME/.local/bin
export GOPATH=$HOME/projects/go
export PATH=$PATH:$GOPATH/bin
export EDITOR="emacsclient -t"
export LESS="$LESS -FRXK"
export WORDCHARS='*?_[]~=&;!#$%^(){}'

source ~/.zplug/init.zsh

zplug "plugins/git-fast", from:oh-my-zsh
zplug "zsh-users/zsh-autosuggestions"
zplug "$HOME", from:local, as:theme, use:".zsh_theme"

export HISTFILE=~/.zsh_history
export HISTORY_IGNORE="(ls*|mpv*|pwd|(go)?pass*|un(zip|rar)*|rm(dir)?*)"
export HISTSIZE=1000
export SAVEHIST=$HISTSIZE

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt APPEND_HISTORY
setopt SHARE_HISTORY
setopt EXTENDEDGLOB

[ -f ~/.aliases ] && source ~/.aliases
_source_if_exists ~/.zcolors
[ -f ~/.zdircolors ] && eval `dircolors ~/.zdircolors` && zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*' menu select

_source_if_exists "/usr/share/zsh/site-functions/_fzf"
_source_if_exists "/usr/share/zsh/site-contrib/fzf.zsh"

export FZF_BIND_OPTS="--bind ctrl-n:up,ctrl-t:down"
export FZF_DEFAULT_OPTS="$FZF_BIND_OPTS --height 7"
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

bindkey '^F' fzf-file-widget
bindkey '^E' kill-word
bindkey '^H' backward-word
bindkey '^T' down-line-or-history
bindkey '^N' up-line-or-history
stty ixoff -ixon && bindkey '^S' forward-word
bindkey '^P' copy-prev-shell-word
bindkey '^[[P' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[H' beginning-of-line
bindkey '^H' backward-delete-char

zplug load
