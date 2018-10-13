hallabro/dotfiles
=================

My configuration of bspwm, emacs, zsh and other stuff.

Installation
============

Install each desired package using [stow](https://www.gnu.org/software/stow/).

* bspwm, compton, neomutt, polybar, rofi, rofi-pass, spacemacs, ssh, sxhkd, zsh:

```bash
stow {bspwm,compton,neomutt,polybar,rofi,rofi-pass,spacemacs,ssh,sxhkd,zsh} -t $HOME
```

* Portage:

```bash
stow portage -t /etc/portage
```

* SDDM, X11:

```bash
stow {sddm,x11} -t /
```
