#!/bin/bash

# find the available names/labels with the following
# for i in /sys/class/hwmon/hwmon*/temp*_input; do
#   echo "{\"name\":\"$(<$(dirname "$i")/name)\",\"label\":\"$(cat "${i%_*}"_label 2>/dev/null ||\
#     echo $(basename "${i%_*}"))\",\"path\":\"""$i""\"}"
# done | jq --compact-output

HWMON_NAME="$1"
HWMON_LABEL="$2"

HWMON_TEMP_FILE_PATH=$(
  for i in /sys/class/hwmon/hwmon*/temp*_input; do
    dir=${i%/*}
    [[ $(<"$dir/name") == "$HWMON_NAME" ]] || continue

    label_file=${i%_*}_label
    if [[ -r "$label_file" ]]; then
      label=$(<"$label_file")
    else
      label=$(basename "${i%_*}")
    fi

    [[ "$label" == "$HWMON_LABEL" ]] && {
      printf '%s\n' "$i"
      break
    }
  done
)
HWMON_TEMP=$(($(< "${HWMON_TEMP_FILE_PATH}") / 1000))

printf '{"text":%s}\n' "${HWMON_TEMP}"
