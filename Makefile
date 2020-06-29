STOW_DEFAULT_ARGS:=--verbose
STOW_COMMAND:=/usr/bin/stow
SHELL=/bin/zsh
HOME:=${HOME}
X11CONF:=/etc/X11
ETC:=/etc
.DEFAULT_GOAL := error

define stow
	${STOW_COMMAND} ${STOW_DEFAULT_ARGS} "${1}" -t ${2}
endef

define stow_dest
	${STOW_COMMAND} ${STOW_DEFAULT_ARGS} --dir=${1} "${2}" -t ${3}
endef

define createmaybe
	mkdir --parents --verbose "${1}"
endef

define sudo_stow
	sudo ${STOW_COMMAND} ${STOW_DEFAULT_ARGS} "${1}" -t ${2}
endef

define sudo_stow_dest
	sudo ${STOW_COMMAND} ${STOW_DEFAULT_ARGS} --dir=${1} "${2}" -t ${3}
endef

define sudo_createmaybe
	sudo mkdir --parents --verbose "${1}"
endef

.PHONY : configexists
configexists:
	$(call createmaybe,${HOME}/.config)

.PHONY : emacs
emacs:
	$(call createmaybe,${HOME}/.doom.d)
	$(call stow,$@,${HOME})
	git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
	~/.emacs.d/bin/doom sync

.PHONY : git
git:
	$(call stow,$@,${HOME})

.PHONY : sxhkd
sxhkd: configexists
	$(call stow,$@,${HOME})

.PHONY : ranger
ranger: configexists
	$(call stow,$@,${HOME})

.PHONY : rofi
rofi: configexists
	$(call createmaybe,${HOME}/.local/share)
	$(call stow,$@,${HOME})

.PHONY : mpv
mpv: configexists
	$(call stow,$@,${HOME})

.PHONY : ssh
ssh:
	$(call createmaybe,${HOME}/.ssh)
	$(call stow,$@,${HOME})

.PHONY : zsh
zsh:
	$(call stow,$@,${HOME})
	curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
	zplug install

.PHONY : less
less:
	lesskey -o ${HOME}/.less less/.lesskey

.PHONY : gtk
gtk: configexists
	$(call stow,$@,${HOME})

.PHONY : sway
gtk: configexists
	$(call stow,$@,${HOME})

.PHONY : base
base: ranger ssh emacs git less zsh

.PHONY : gui
gui: sway rofi mpv gtk

.PHONY : desktop
desktop: base gui

.PHONY : t25
t25: base gui

.PHONY : fileserver
fileserver: base

.PHONY : error
error:
	@echo "usage: make <profile>"
