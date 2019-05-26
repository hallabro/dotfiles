export ZSH=$HOME/.oh-my-zsh
export PATH=$PATH:$HOME/.local/bin
export GOPATH=$HOME/projects/go
export PATH=$PATH:$GOPATH/bin
export EDITOR="emacsclient -t"

ZSH_THEME="hallabro"
plugins=(git)
alias alias="true"
source $ZSH/oh-my-zsh.sh
unalias "alias" # hackish way of disabling all bundled aliases

export HISTORY_IGNORE="(ls*|mpv*|pwd|pass*|un(zip|rar)*|rm*)"
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
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
[ -f ~/.zcolors ] && source ~/.zcolors
[ -f ~/.zdircolors ] && eval `dircolors ~/.zdircolors` && zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

FZF=/usr/share/zsh/site-contrib/fzf.zsh
[ -f $FZF ] && source $FZF
export FZF_BIND_OPTS="--bind ctrl-n:up,ctrl-t:down"
export FZF_DEFAULT_OPTS="$FZF_BIND_OPTS --height 10"

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}
