#!/bin/bash

BROWSER=brave
BROWSER_TERMINATION_WAIT_SECONDS=1.5

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
if [[ "$XDG_SESSION_TYPE" == "wayland" ]]; then
  # WAYLAND (HYPRLAND + UWSM) ACTIONS
  # We send SIGTERM to the browser and wait to let it save its session properly
  ACTIONS[shutdown]="systemd-run --user --no-block bash -c 'pkill -TERM ${BROWSER}; sleep ${BROWSER_TERMINATION_WAIT_SECONDS}; uwsm stop; systemctl poweroff'"
  ACTIONS[reboot]="systemd-run --user --no-block bash -c 'pkill -TERM ${BROWSER}; sleep ${BROWSER_TERMINATION_WAIT_SECONDS}; uwsm stop; systemctl reboot'"
  ACTIONS[suspend]="systemctl suspend"
  ACTIONS[hibernate]="systemctl hibernate"
  ACTIONS[logout]="systemd-run --user --no-block bash -c 'pkill -TERM ${BROWSER}; sleep ${BROWSER_TERMINATION_WAIT_SECONDS}; uwsm stop'"
  ACTIONS[lockscreen]="loginctl lock-session ${XDG_SESSION_ID}"
else
  # X11 (XMONAD) / STANDARD ACTIONS
  ACTIONS[shutdown]="systemctl poweroff"
  ACTIONS[reboot]="systemctl reboot"
  ACTIONS[suspend]="systemctl suspend"
  ACTIONS[hibernate]="systemctl hibernate"
  ACTIONS[logout]="loginctl kill-session ${XDG_SESSION_ID}"
  ACTIONS[lockscreen]="loginctl lock-session ${XDG_SESSION_ID}"
fi

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

eval "${ACTIONS[$SELECTED]}"
