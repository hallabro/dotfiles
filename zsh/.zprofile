if [[ -x "$(command -v gnome-keyring-daemon)" ]]; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

if [[ $TTY = "/dev/tty1" && ! $DISPLAY ]]; then
    exec sway
fi
