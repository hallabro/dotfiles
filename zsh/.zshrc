export PATH=$PATH:$HOME/.local/bin
export GOPATH=$HOME/projects/go
export PATH=$PATH:$GOPATH/bin
export EDITOR="emacsclient -t"
export LESS="$LESS -FRXK"
export WORDCHARS='*?_[]~=&;!#$%^(){}'

source ~/.zplug/init.zsh

zplug "plugins/git-fast", from:oh-my-zsh
zplug "aperezdc/zsh-fzy"
zplug "~/dotfiles", from:local, as:theme, use:"zsh/.zsh_theme"

export HISTFILE=~/.zsh_history
export HISTORY_IGNORE="(ls*|mpv*|pwd|(go)?pass*|un(zip|rar)*|rm(dir)?*)"
export HISTSIZE=1000
export SAVEHIST=$HISTSIZE

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt SHARE_HISTORY
setopt EXTENDEDGLOB

[ -f ~/.aliases ] && source ~/.aliases
_source_if_exists ~/.zcolors
[ -f ~/.zdircolors ] && eval `dircolors ~/.zdircolors` && zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle :fzy:file command fd --type f

bindkey '^F' fzy-file-widget
bindkey '^R' fzy-history-widget
bindkey '^E' kill-word
bindkey '^H' backward-word
bindkey '^J' down-line-or-history
bindkey '^K' up-line-or-history
bindkey '^L' forward-word
bindkey '^P' copy-prev-shell-word

zplug load
