STOW_DEFAULT_ARGS:=--verbose
STOW_COMMAND:=/usr/local/bin/stow
SHELL=/usr/local/bin/zsh
HOME:=${HOME}
.DEFAULT_GOAL := git ssh zsh

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

.PHONY : git
git:
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
