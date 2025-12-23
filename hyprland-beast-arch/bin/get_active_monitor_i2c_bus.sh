#!/usr/bin/env bash

# get cursor
eval $(hyprctl cursorpos -j | jq -r '"CX=\(.x) CY=\(.y)"')

# find active monitor
conn=$(
  hyprctl monitors -j |
    jq -r --argjson cx "$CX" --argjson cy "$CY" '
      .[] | select(
        ($cx >= .x) and
        ($cx < (.x + .width)) and
        ($cy >= .y) and
        ($cy < (.y + .height))
      ) | .name
    '
)

# extract card (e.g. "card1") from matching sysfs entry
card=$(basename "$(dirname "$(readlink -f "/sys/class/drm"/*-$conn)")")

# resolve i2c bus through DDC link
i2c=$(readlink "/sys/class/drm/$card-$conn/ddc" | sed 's#^.*/##')

# print just the numeric part
echo "${i2c#i2c-}"
