export ZSH=$HOME/.oh-my-zsh
export PATH=$PATH:$HOME/.local/bin

ZSH_THEME="theunraveler"
plugins=(git)

source $ZSH/oh-my-zsh.sh

export HISTORY_IGNORE="(ls|mpv *|pwd|pass *|unar *|unzip *)"
export HISTSIZE=200
export SAVEHIST=$HISTSIZE

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt SHARE_HISTORY

[ -f ~/.aliases ] && source ~/.aliases
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
