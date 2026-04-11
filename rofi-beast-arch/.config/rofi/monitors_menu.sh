#!/bin/bash

RELATIVE_CONFIG_DIR=".config/hypr"
CONFIG_DIR="${HOME}/${RELATIVE_CONFIG_DIR}"
HYPRLAND_CONF="${CONFIG_DIR}/hyprland.conf"

MONITORS_OPTIONS=(
  work
  gaming
)
MONITORS_OPTIONS_LENGTH=${#MONITORS_OPTIONS[@]}

declare -A ICONS
ICONS[work]=""
ICONS[gaming]=""

declare -A ICON_COLORS
ICON_COLORS[work]="#458588"
ICON_COLORS[gaming]="#d65d0e"

declare -A LABELS
LABELS[work]="Work (dual)"
LABELS[gaming]="Gaming (single)"

SEP=""
SELECTED_INDEX=$(
  for key in "${MONITORS_OPTIONS[@]}"; do
    printf "%s%s\0icon\x1f%s" \
      "${SEP}" \
      "${LABELS[${key}]}" \
      "<span color='${ICON_COLORS[${key}]}'>${ICONS[${key}]}</span>"
    SEP="|"
  done | \
  rofi \
    -sep '|' \
    -l "${MONITORS_OPTIONS_LENGTH}" \
    -theme "${HOME}/.config/rofi/monitors_menu.rasi" \
    -dmenu -i -format i -p "Monitors menu"
)

if [[ -z "${SELECTED_INDEX}" ]]; then
  echo "No config selected"
  exit 0
fi

SELECTED="${MONITORS_OPTIONS[$SELECTED_INDEX]}"

echo "SELECTED_INDEX: ${SELECTED_INDEX}"
echo "SELECTED: ${SELECTED}"

echo "Update ${HYPRLAND_CONF}..."
sed --follow-symlinks \
  --in-place "s|^source = ~/${RELATIVE_CONFIG_DIR}/monitors_.*|source = ~/${RELATIVE_CONFIG_DIR}/monitors_${SELECTED}.conf|" \
  "${HYPRLAND_CONF}"
