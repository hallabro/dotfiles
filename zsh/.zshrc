export ZSH=$HOME/.oh-my-zsh
export PATH=$PATH:$HOME/.local/bin
export GOPATH=$HOME/projects/go
export PATH=$PATH:$GOPATH/bin

ZSH_THEME="theunraveler"
plugins=(git)
source $ZSH/oh-my-zsh.sh
unalias -m "g*" # the git plugin has way to many aliases

export HISTORY_IGNORE="(ls|mpv *|pwd|pass *|unar *|unzip *)"
export HISTSIZE=1000
export SAVEHIST=$HISTSIZE

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"
unalias -m "base16*"

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
[ -f ~/.zdircolors ] && eval `dircolors .zdircolors` && zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

FZF=/usr/share/zsh/site-contrib/fzf.zsh
[ -f $FZF ] && source $FZF

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}
_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
}
