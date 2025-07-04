#!/bin/bash

POWER_MENU_OPTIONS=(lockscreen shutdown reboot logout)

declare -A LABELS
LABELS[lockscreen]="󰌾  Lock screen"
LABELS[shutdown]="󰐥  Shutdown"
LABELS[reboot]="󰜉  Reboot"
LABELS[logout]="󰍃  Logout"

declare -A ACTIONS
ACTIONS[lockscreen]="loginctl lock-session ${XDG_SESSION_ID-}"
ACTIONS[shutdown]="systemctl poweroff"
ACTIONS[reboot]="systemctl reboot"
ACTIONS[logout]="hyprctl dispatch exit"

SORTED_LABELS=()
for key in "${POWER_MENU_OPTIONS[@]}"; do
  SORTED_LABELS+=("${LABELS[$key]}")
done

JOINED_LABELS=$( IFS='|'; echo "${SORTED_LABELS[*]}" )

# using `echo -n` otherwise the last item would contain a trailing newline, messing up the vertical alignment
# ref: https://github.com/davatorium/rofi/issues/993#issuecomment-509659940
SELECTED_INDEX=$(
  echo -n "${JOINED_LABELS}" |\
  rofi -sep '|' -theme "${HOME}/.config/rofi/power_menu.rasi" -dmenu -i -format i -p "Power menu"
)
SELECTED="${POWER_MENU_OPTIONS[$SELECTED_INDEX]}"

if [[ -z "${SELECTED_INDEX}" ]]; then
  echo "No action selected"
else
  echo "SELECTED_INDEX: ${SELECTED_INDEX}"
  echo "SELECTED: ${SELECTED}"
  echo "ACTION: ${ACTIONS[$SELECTED]}"
  ${ACTIONS[$SELECTED]}
fi
