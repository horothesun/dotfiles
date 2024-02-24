#!/bin/bash

# Xmobar: set TFL_APP_KEY value in ~/.profile and ~/.zshenv

[[ -z "${TFL_APP_KEY}" ]] && echo "Error: TFL_APP_KEY undefined" && exit 100

FROM_STOP_POINT_ID="$1"
[[ -z "${FROM_STOP_POINT_ID}" ]] && echo "Error: FROM_STOP_POINT_ID must be passed as first argument" && exit 101

TO_STOP_POINT_ID="$2"
[[ -z "${TO_STOP_POINT_ID}" ]] && echo "Error: TO_STOP_POINT_ID must be passed as second argument" && exit 102

JOURNEY_DATE="$3"
[[ -z "${JOURNEY_DATE}" ]] && echo "Error: JOURNEY_DATE must be passed as third argument" && exit 103

CURRENT_INSTANT=$(date +%s)

curl --silent \
  "https://api.tfl.gov.uk/Journey/JourneyResults/${FROM_STOP_POINT_ID}/to/${TO_STOP_POINT_ID}?date=${JOURNEY_DATE}&app_key=${TFL_APP_KEY}" | \
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
