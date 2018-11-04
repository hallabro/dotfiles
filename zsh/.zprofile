if [[ -x "$(command -v keychain)" ]]; then
    eval $(keychain ~/.ssh/id_rsa B4C48F45F16E612B625004B802E221882616B4EB \
        --agents ssh,gpg \
	--eval \
	--noask \
	--quiet)
fi

if [[ -x "$(command -v dbus-launch)" ]]; then
    export $(dbus-launch)
fi

if [[ $TTY = "/dev/tty1" && ! $DISPLAY ]]; then
    exec startx
fi
