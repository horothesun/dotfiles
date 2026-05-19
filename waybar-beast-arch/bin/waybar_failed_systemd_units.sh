#!/bin/bash

FAILED_SYSTEM_UNITS=$(systemctl --state=failed --output=json | jq --compact-output 'map(.unit)')
FAILED_USER_UNITS=$(systemctl --user --state=failed --output=json | jq --compact-output 'map(.unit)')
echo "{ \"failed_system_units\": ${FAILED_SYSTEM_UNITS}, \"failed_user_units\": ${FAILED_USER_UNITS} }" | jq '.'
