#!/bin/bash

# Xmobar: set TFL_APP_KEY, HOME_STATION_ID and WORK_STATION_ID value in ~/.profile and ~/.zshenv

[[ -z "${TFL_APP_KEY}" ]] && echo "Error: TFL_APP_KEY undefined" && exit 101

[[ -z "${HOME_STATION_ID}" ]] && echo "Error: HOME_STATION_ID undefined" && exit 102
[[ -z "${WORK_STATION_ID}" ]] && echo "Error: WORK_STATION_ID undefined" && exit 103


TFL_BASE_URL="https://api.tfl.gov.uk"

function get_journey_start_times() {
  FROM_STOP_POINT_ID="$1"
  TO_STOP_POINT_ID="$2"
  JOURNEY_DATE="$3"
  CURRENT_INSTANT=$(date +%s)
  curl --silent \
    "${TFL_BASE_URL}/Journey/JourneyResults/${FROM_STOP_POINT_ID}/to/${TO_STOP_POINT_ID}?date=${JOURNEY_DATE}&app_key=${TFL_APP_KEY}" | \
    jq \
      --arg fromStopPointId "${FROM_STOP_POINT_ID}" \
      --arg toStopPointId "${TO_STOP_POINT_ID}" \
      --arg currentInstant "${CURRENT_INSTANT}" '[
        .journeys[]
      | select(
          any(
            .legs[];
            .departurePoint.naptanId == "\($fromStopPointId)"
              and (.arrivalPoint.naptanId == "\($toStopPointId)")
          )
        )
      | "\(.startDateTime)Z"
      | select((. | fromdateiso8601) > ($currentInstant | tonumber))
      | split("T")[1]
      | split(":")[0:2]
      | join(":")
    ]' 2> /dev/null || echo "[]"
}

JOURNEY_DATE=$(date -u +%Y%m%d)

get_journey_start_times "${WORK_STATION_ID}" "${HOME_STATION_ID}" "${JOURNEY_DATE}" | \
  jq --raw-output 'if (length > 0) then .[0] else "N/A" end'
