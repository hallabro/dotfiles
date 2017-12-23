export ZSH=$HOME/.oh-my-zsh
export PATH=$PATH:$HOME/.local/bin
export HISTORY_IGNORE="(ls|mpv *|pwd|pass *|electrum *|unar *|unzip *)"

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

setopt HIST_IGNORE_SPACE

ZSH_THEME="theunraveler"
plugins=(git)
setopt hist_ignore_dups

source $ZSH/oh-my-zsh.sh

[ -f ~/.aliases ] && source ~/.aliases
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
