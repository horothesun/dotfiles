#!/bin/bash

POWER_MENU_OPTIONS=(
  shutdown
  reboot
  suspend
  #hibernate
  logout
  lockscreen
)
POWER_MENU_OPTIONS_LENGTH=${#POWER_MENU_OPTIONS[@]}

declare -A ICONS
ICONS[shutdown]=""
ICONS[reboot]="󰜉"
ICONS[suspend]="󰒲"
ICONS[hibernate]="󰋊"
ICONS[logout]="󰍃"
ICONS[lockscreen]="󰌾"

declare -A ICON_COLORS
ICON_COLORS[shutdown]="#cc241d"
ICON_COLORS[reboot]="#d65d0e"
ICON_COLORS[suspend]="#b16286"
ICON_COLORS[hibernate]="#458588"
ICON_COLORS[logout]="#d79921"
ICON_COLORS[lockscreen]="#689d6a"

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
    printf "%s%s\0icon\x1f%s" \
      "${SEP}" \
      "${LABELS[${key}]}" \
      "<span color='${ICON_COLORS[${key}]}'>${ICONS[${key}]}</span>"
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
