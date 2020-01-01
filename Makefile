STOW_DEFAULT_ARGS:=--verbose
STOW_COMMAND:=/usr/bin/stow
SHELL=/bin/zsh
HOME:=${HOME}
PORTAGE:=/etc/portage
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

.PHONY : bspwm
bspwm: configexists
	$(call stow,$@,${HOME})

.PHONY : dunst
dunst: configexists
	$(call stow,$@,${HOME})

.PHONY : emacs
emacs: 
	$(call createmaybe,${HOME}/.emacs.d)
	$(call stow,$@,${HOME})

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

.PHONY : rofi-pass
rofi-pass: rofi configexists
	$(call stow,$@,${HOME})

.PHONY : mpv
mpv: configexists
	$(call stow,$@,${HOME})

.PHONY : parcellite
parcellite: configexists
	$(call stow,$@,${HOME})

.PHONY : ssh
ssh:
	$(call createmaybe,${HOME}/.ssh)
	$(call stow,$@,${HOME})

.PHONY : zsh
zsh:
	$(call stow,$@,${HOME})

.PHONY : less
less:
	lesskey -o ${HOME}/.less less/.lesskey

.PHONY : compton
compton: configexists
	$(call stow,$@,${HOME})

.PHONY : dracut
dracut:
	$(call sudo_stow,$@,${ETC})

.PHONY : gtk
gtk: configexists
	$(call stow,$@,${HOME})

.PHONY : x11conf
x11conf:
	$(call sudo_createmaybe,${X11CONF}/xinit/xinitrc.d)
	$(call sudo_createmaybe,${X11CONF}/xorg.conf.d)
	$(call sudo_stow,x11,${X11CONF})

.PHONY : x11_desktop
x11_desktop: x11conf
	$(call sudo_stow_dest,desktop,x11,${X11CONF})

.PHONY : x11_t25
x11_t25: x11conf
	$(call sudo_stow_dest,t25,x11,${X11CONF})

.PHONY : portage_desktop
portage_desktop: portage
	$(call sudo_stow_dest,desktop,portage,${PORTAGE})

.PHONY : portage_t25
portage_t25: portage
	$(call sudo_stow_dest,t25,portage,${PORTAGE})

.PHONY : portage_fileserver
portage_fileserver: portage
	$(call sudo_stow_dest,fileserver,portage,${PORTAGE})

.PHONY : portage
portage:
	$(call sudo_createmaybe,${PORTAGE}/make.conf)
	$(call sudo_createmaybe,${PORTAGE}/package.keywords)
	$(call sudo_createmaybe,${PORTAGE}/package.license)
	$(call sudo_createmaybe,${PORTAGE}/package.use)
	$(call sudo_createmaybe,${PORTAGE}/repos.conf)
	$(call sudo_stow,$@,${PORTAGE})

.PHONY : base
base: sxhkd ranger ssh emacs git less zsh

.PHONY : x11
x11: bspwm parcellite rofi rofi-pass dunst mpv compton gtk

.PHONY : desktop
desktop: base x11 x11_desktop portage_desktop

.PHONY : t25
t25: base x11 x11conf x11_t25 portage_t25

.PHONY : fileserver
fileserver: base portage_fileserver

.PHONY : error
error:
	@echo "usage: make <profile>"
