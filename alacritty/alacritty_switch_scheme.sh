#!/bin/bash

ALACRITTY_CONFIG_FILE=~/dotfiles/alacritty/.alacritty.yml

SCHEMES=$(yq eval --tojson $ALACRITTY_CONFIG_FILE | jq -r '.schemes | keys | .[]')
CURRENT_SCHEME=$(grep 'colors: \*' $ALACRITTY_CONFIG_FILE | cut -d '*' -f2)
CURRENT_DRAW_BOLD_TEXT_WITH_BRIGHT_COLORS=$(grep 'draw_bold_text_with_bright_colors: ' $ALACRITTY_CONFIG_FILE | cut -d ' ' -f2)

echo "Available schemes:"
echo ""
for SCHEME in $SCHEMES; do echo $SCHEME; done
echo ""
echo "IMPORTANT: this script is going to modify the following Alacritty configuration file"
echo "           $ALACRITTY_CONFIG_FILE"
echo ""

read -p "colors [$CURRENT_SCHEME]: " SELECTED_SCHEME
read -p "draw_bold_text_with_bright_colors [$CURRENT_DRAW_BOLD_TEXT_WITH_BRIGHT_COLORS]: " SELECTED_DRAW_BOLD_TEXT_WITH_BRIGHT_COLORS

SELECTED_SCHEME=${SELECTED_SCHEME:-$CURRENT_SCHEME}
SELECTED_DRAW_BOLD_TEXT_WITH_BRIGHT_COLORS=${SELECTED_DRAW_BOLD_TEXT_WITH_BRIGHT_COLORS:-$CURRENT_DRAW_BOLD_TEXT_WITH_BRIGHT_COLORS}

ex \
  -c "%s/^draw_bold_text_with_bright_colors: .*/draw_bold_text_with_bright_colors: $SELECTED_DRAW_BOLD_TEXT_WITH_BRIGHT_COLORS" \
  -c "%s/^colors: \*.*/colors: \*$SELECTED_SCHEME" \
  -c "wq" $ALACRITTY_CONFIG_FILE
