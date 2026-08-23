#!/bin/bash

DDCUTIL_DETECT_JSON=$( ddcutil detect | awk '
BEGIN {
    print "{"
    first = 1
}

# Extract the numeric I2C bus ID
/I2C bus:/ {
    bus = $NF
    sub(/\/dev\/i2c-/, "", bus)
}

# Extract the Serial Number (ignoring the "Binary" line)
/^[[:space:]]*Serial number:/ {
    if ($0 ~ /Binary/) next;

    # Isolate the value after the colon
    idx = index($0, ":")
    sn = substr($0, idx + 1)

    # Trim whitespace
    sub(/^[[:space:]]+/, "", sn)
    sub(/[[:space:]]+$/, "", sn)

    # Print as a JSON key-value pair
    if (!first) {
        print ","
    }
    printf "  \"%s\": %s", sn, bus
    first = 0
}

END {
    print "\n}"
}
'
)

echo "${DDCUTIL_DETECT_JSON}" | jq --raw-output '
  to_entries
  | map("  [\"\(.key)\"] = \(.value)")
  | join(",\n")
  | "return {\n\(.)\n}"
'
