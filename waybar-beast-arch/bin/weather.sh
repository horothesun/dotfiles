#!/usr/bin/env bash

# "wttr.in?format=%c%C+|+%t+(%f)+|+%w"
RESPONSE=$(
  curl --silent --write-out "\n%{http_code}" \
    'wttr.in?format=\{"condition_icon":"%c","condition_name":"%C","humidity":"%h","temp_actual":"%t","temp_feels":"%f","wind":"%w","moon_phase":"%m","moon_day":"%M","precipitation":"%p/3h","pressure":"%P","uv_index":"%u","sunrise":"%S","sunset":"%s"\}'
)
HTTP_CODE=$(tail -n1 <<< "${RESPONSE}")
RESPONSE_BODY=$(sed '$d' <<< "${RESPONSE}")

OUTPUT_TEXT='"🌤️ ⁉️"'
OUTPUT_TOOLTIP='"⚠️ Service unreachable"'
if [[ "$HTTP_CODE" == "200" ]]; then
  OUTPUT_TEXT=$(echo "${RESPONSE_BODY}" | jq '"\(.condition_icon)\(.temp_actual | gsub("[+C]"; "")) (\(.temp_feels | gsub("[+C]"; "")))"')
  OUTPUT_TOOLTIP=$(echo "${RESPONSE_BODY}" | jq '"Condition: \(.condition_icon)\(.condition_name)\nTemperature: \(.temp_actual | gsub("[+]"; ""))\nFeels like: \(.temp_feels | gsub("[+]"; ""))\nWind: \(.wind)\nHumidity: \(.humidity)\nPrecipitation: \(.precipitation)\nPressure: \(.pressure)\nUV index: \(.uv_index)\nSunrise: \(.sunrise)\nSunset: \(.sunset)\nMoon phase: \(.moon_phase)\nMoon day: \(.moon_day)"')
fi

echo "{
    \"text\": ${OUTPUT_TEXT},
    \"tooltip\": ${OUTPUT_TOOLTIP}
  }" |\
  jq --monochrome-output --unbuffered --compact-output
