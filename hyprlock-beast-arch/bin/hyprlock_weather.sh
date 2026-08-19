#!/usr/bin/env bash

CURL_TIMEOUT_SECONDS=5
CURL_MAXIMUM_ATTEMPTS=10

OUTPUT_TEXT="🌤️ ⁉️"

for ((i=1; i <= CURL_MAXIMUM_ATTEMPTS; i++)); do
  RESPONSE=$(
    curl --silent --fail --connect-timeout "${CURL_TIMEOUT_SECONDS}" --write-out "\n%{http_code}" \
      'wttr.in?format=\{"condition_icon":"%c","condition_name":"%C","humidity":"%h","temp_actual":"%t","temp_feels":"%f","wind":"%w","moon_phase":"%m","moon_day":"%M","precipitation":"%p/3h","pressure":"%P","uv_index":"%u","sunrise":"%S","sunset":"%s"\}'
  )

  HTTP_CODE=$(tail -n1 <<< "${RESPONSE}")
  RESPONSE_BODY=$(sed '$d' <<< "${RESPONSE}")

  if [[ "$HTTP_CODE" == "200" ]]; then
    OUTPUT_TEXT=$(
      echo "${RESPONSE_BODY}" | \
        jq --raw-output '
            "\(.condition_icon)\(.temp_actual | gsub("[+C]"; "")) "
          + "(\(.temp_feels | gsub("[+C]"; "")))  "
          + "\(.wind)"
        '
    )

    break
  fi

  sleep "${CURL_TIMEOUT_SECONDS}"
done

echo "${OUTPUT_TEXT}"
