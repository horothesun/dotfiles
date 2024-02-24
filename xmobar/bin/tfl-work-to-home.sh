#!/bin/bash

# Xmobar: set TFL_APP_KEY, HOME_STATION_ID and WORK_STATION_ID value in ~/.profile and ~/.zshenv

[[ -z "${HOME_STATION_ID}" ]] && echo "Error: HOME_STATION_ID undefined" && exit 101
[[ -z "${WORK_STATION_ID}" ]] && echo "Error: WORK_STATION_ID undefined" && exit 102

JOURNEY_DATE=$(date -u +%Y%m%d)

tfl_get_journey_start_times.sh "${WORK_STATION_ID}" "${HOME_STATION_ID}" "${JOURNEY_DATE}" | \
  jq --raw-output 'if (length > 0) then .[0] else "N/A" end'
