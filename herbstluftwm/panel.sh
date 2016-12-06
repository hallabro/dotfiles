#!/bin/bash

set -f
monitor=${1:-0}

PANEL_DISPLAY="0"
PANEL_FONT="Terminus:size=8"
PANEL_HEIGHT=18

DISPLAY_WIDTH=$(herbstclient monitor_rect $PANEL_DISPLAY | cut -d" " -f 3)
PANEL_DIMENSIONS="${DISPLAY_WIDTH}x${PANEL_HEIGHT}"

PANEL_BACKGROUND_COLOR="#282828"
ACTIVE_TAG_BACKGROUND_COLOR="#383838"
TEXT_DARK_COLOR="#B8B8B8"
TEXT_LIGHT_COLOR="#E8E8E8"

herbstclient pad $PANEL_DISPLAY $PANEL_HEIGHT
{
    mpc idleloop 2> /dev/null &
    MPC_PID=$!

    herbstclient --idle

    kill $MPC_PID

} 2> /dev/null | {
    read -r TAGS <<< "$(herbstclient tag_status $PANEL_DISPLAY)"
    NOW_PLAYING=$(mpc current 2> /dev/null)

    while true; do
        for i in ${TAGS}; do
            ACTIVE_FORMAT_START=
            ACTIVE_FORMAT_END=

            case ${i:0:1} in
                '#') # current tag
                    ACTIVE_FORMAT_START="%{B${ACTIVE_TAG_BACKGROUND_COLOR}}%{F${TEXT_LIGHT_COLOR}}"
                    ACTIVE_FORMAT_END="%{F-}%{B-}"
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
            echo -n "$ACTIVE_FORMAT_START  $TAG_NAME  $ACTIVE_FORMAT_END"
        done

        echo -n "%{r}"
        echo -n "$NOW_PLAYING "

        echo # a new line is required

        read -r CMD ARGUMENTS || break
        case "${CMD}" in
            tag_changed)
                read -r TAGS <<< "$(herbstclient tag_status)"
                ;;
            quit_panel)
                exit
                ;;
            player)
                read -r NOW_PLAYING <<< "$(mpc current 2> /dev/null)"
                ;;
            reload)
                exit
                ;;
        esac
    done
} 2> /tmp/lemonbar.err | lemonbar $1 -f "$PANEL_FONT" -g "$PANEL_DIMENSIONS" -p -B "$PANEL_BACKGROUND_COLOR" -F "$TEXT_DARK_COLOR"
