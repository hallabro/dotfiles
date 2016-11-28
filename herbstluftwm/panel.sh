#!/bin/bash

set -f
monitor=${1:-0}
separator="\f0  |  \fr"

herbstclient pad 0 16
{
    while true ; do
        date +'date_day %A %e.  '
        date +'date_min %H:%M  '
        sleep 1 || break
    done > >(uniq_linebuffered) &
    date_pid=$!

    herbstclient --idle

    kill $date_pid
} 2> /dev/null | {
    TAGS=( $(herbstclient tag_status) )
    date_day=""
    date_min=""
    visible=true

    while true ; do
        echo -n "%{c}"
        for i in "${TAGS[@]}" ; do
            FORMAT_START=
            FORMAT_END=
            case ${i:0:1} in
                '#') # current tag
                    FORMAT_START='%{+u}'
                    FORMAT_END='%{-u}'
                    ;;
                '+') # active on other monitor
                    ;;
                ':')
                    ;;
                '!') # urgent tag
                    ;;
                *)
                    ;;
            esac
            TAG_NAME="`echo ${i:1} | tr '[:upper:]' '[:lower:]'`"
            echo -n " $FORMAT_START$TAG_NAME$FORMAT_END "
        done
        # align right
        echo
        # wait for next event
    read line || break
        case "${cmd[0]}" in
            tag*)
                TAGS=( $(herbstclient tag_status) )
                ;;
            date_day)
                date_day="${cmd[@]:1}"
                ;;
            date_min)
                date_min="${cmd[@]:1}"
                ;;
            #player)
                #song=$(get_mpd_song)
                #;;
            quit_panel)
                exit
                ;;
            reload)
                exit
                ;;
        esac
    cmd=( $line )
    done
} 2> /dev/null | lemonbar $1 -f "Terminus:size=8" -g 1920x16-0+1060 -p -B "#282828"
