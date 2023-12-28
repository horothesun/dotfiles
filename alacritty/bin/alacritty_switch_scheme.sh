#!/bin/bash

ALACRITTY_CONFIG_FILE=~/dotfiles/alacritty/.alacritty.toml
ALACRITTY_COLOR_SCHEMES_FILE=~/dotfiles/alacritty/alacritty_color_schemes.toml

COLOR_SCHEMES_JSON=$(
  dasel --file "${ALACRITTY_COLOR_SCHEMES_FILE}" --write=json '.' | \
    jq --compact-output '.'
)

SCHEMES=$(echo "${COLOR_SCHEMES_JSON}" | jq --raw-output '.schemes | keys[]')

echo "Available schemes:"
echo ""
for SCHEME in $SCHEMES; do echo "${SCHEME}"; done
echo ""
echo "IMPORTANT: this script is going to modify the following Alacritty configuration file"
echo "           $ALACRITTY_CONFIG_FILE"
echo ""

read -r -p "New scheme: " SELECTED_SCHEME
echo ""

SELECTED_COLOR_SCHEME_JSON=$(
  echo "${COLOR_SCHEMES_JSON}" | \
    jq --compact-output --arg scheme "${SELECTED_SCHEME}" '.schemes.[$scheme] | { "colors": . }'
)

echo "Selected scheme: ${SELECTED_SCHEME}"
echo ""
echo "${SELECTED_COLOR_SCHEME_JSON}" | dasel --read=json --write=toml --colour '.'
echo ""

function set_live_config_reload() {
  echo ".live_config_reload := $1"
  dasel put \
    --file "${ALACRITTY_CONFIG_FILE}" \
    --type="bool" \
    --value="$1" \
    '.live_config_reload'
}

function put() {
  ALACRITTY_CONFIG_TYPE="$1"
  ALACRITTY_CONFIG_KEY="$2"
  ALACRITTY_CONFIG_VALUE=$(echo "${SELECTED_COLOR_SCHEME_JSON}" | jq --raw-output "${ALACRITTY_CONFIG_KEY}")
  echo "${ALACRITTY_CONFIG_KEY} (${ALACRITTY_CONFIG_TYPE}) := ${ALACRITTY_CONFIG_VALUE}"
  dasel put \
    --file "${ALACRITTY_CONFIG_FILE}" \
    --type="${ALACRITTY_CONFIG_TYPE}" \
    --value="${ALACRITTY_CONFIG_VALUE}" \
    "${ALACRITTY_CONFIG_KEY}"
}

echo "Updating ${ALACRITTY_CONFIG_FILE}..."
echo ""

set_live_config_reload false
echo ""

put "bool" ".colors.draw_bold_text_with_bright_colors"
echo ""

put "string" ".colors.bright.black"
put "string" ".colors.bright.blue"
put "string" ".colors.bright.cyan"
put "string" ".colors.bright.green"
put "string" ".colors.bright.magenta"
put "string" ".colors.bright.red"
put "string" ".colors.bright.white"
put "string" ".colors.bright.yellow"
echo ""

put "string" ".colors.cursor.cursor"
put "string" ".colors.cursor.text"
echo ""

put "string" ".colors.normal.black"
put "string" ".colors.normal.blue"
put "string" ".colors.normal.cyan"
put "string" ".colors.normal.green"
put "string" ".colors.normal.magenta"
put "string" ".colors.normal.red"
put "string" ".colors.normal.white"
put "string" ".colors.normal.yellow"
echo ""

put "string" ".colors.primary.background"
put "string" ".colors.primary.foreground"
echo ""

set_live_config_reload true
echo ""
