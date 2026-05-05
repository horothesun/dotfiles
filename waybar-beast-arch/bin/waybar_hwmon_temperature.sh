#!/bin/bash

# find the available names/labels with the following
# for i in /sys/class/hwmon/hwmon*/temp*_input; do
#   echo "{\"name\":\"$(<$(dirname "$i")/name)\",\"label\":\"$(cat "${i%_*}"_label 2>/dev/null ||\
#     echo $(basename "${i%_*}"))\", \"path\": \"$(readlink -f "$i")\"}"
# done | jq --compact-output

HWMON_NAME="$1"
HWMON_LABEL="$2"

HWMON_TEMP_FILE_PATH=$(
  for i in /sys/class/hwmon/hwmon*/temp*_input; do
    echo "{\"name\":\"$(<$(dirname "$i")/name)\",\"label\":\"$(cat "${i%_*}"_label 2>/dev/null ||\
      echo $(basename "${i%_*}"))\", \"path\": \"$(readlink -f "$i")\"}"
  done | jq \
      --raw-output \
      --arg hwmonName "${HWMON_NAME}" \
      --arg hwmonLabel "${HWMON_LABEL}" \
      'select(.name == $hwmonName and .label == $hwmonLabel).path'
)
HWMON_TEMP=$(echo "scale = 0; $(cat "${HWMON_TEMP_FILE_PATH}") / 1000" | bc)

echo "{\"text\":${HWMON_TEMP}}"
