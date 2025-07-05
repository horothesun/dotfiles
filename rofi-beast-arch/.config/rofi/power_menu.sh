#!/bin/bash

POWER_MENU_OPTIONS=(shutdown reboot suspend hibernate logout lockscreen)
POWER_MENU_OPTIONS_LENGTH=${#POWER_MENU_OPTIONS[@]}

declare -A ICONS
ICONS[shutdown]="<span color='#cc241d'></span>"
ICONS[reboot]="<span color='#d65d0e'>󰜉</span>"
ICONS[suspend]="<span color='#b16286'>󰒲</span>"
ICONS[hibernate]="<span color='#458588'>󰋊</span>"
ICONS[logout]="<span color='#d79921'>󰍃</span>"
ICONS[lockscreen]="<span color='#689d6a'>󰌾</span>"

declare -A LABELS
LABELS[shutdown]="Shutdown"
LABELS[reboot]="Reboot"
LABELS[suspend]="Suspend"
LABELS[hibernate]="Hibernate"
LABELS[logout]="Logout"
LABELS[lockscreen]="Lock screen"

declare -A ACTIONS
ACTIONS[shutdown]="systemctl poweroff"
ACTIONS[reboot]="systemctl reboot"
ACTIONS[suspend]="systemctl suspend"
ACTIONS[hibernate]="systemctl hibernate"
ACTIONS[logout]="hyprctl dispatch exit"
ACTIONS[lockscreen]="loginctl lock-session ${XDG_SESSION_ID-}"

SEP=""
SELECTED_INDEX=$(
  for key in "${POWER_MENU_OPTIONS[@]}"; do
    printf "%s%s\0icon\x1f%s" "${SEP}" "${LABELS[${key}]}" "${ICONS[$key]}"
    SEP="|"
  done | \
  rofi \
    -sep '|' \
    -l "${POWER_MENU_OPTIONS_LENGTH}" \
    -theme "${HOME}/.config/rofi/power_menu.rasi" \
    -dmenu -i -format i -p "Power menu"
)

if [[ -z "${SELECTED_INDEX}" ]]; then
  echo "No action selected"
  exit 0
fi

SELECTED="${POWER_MENU_OPTIONS[$SELECTED_INDEX]}"

echo "SELECTED_INDEX: ${SELECTED_INDEX}"
echo "SELECTED: ${SELECTED}"
echo "ACTION: ${ACTIONS[$SELECTED]}"

${ACTIONS[$SELECTED]}
