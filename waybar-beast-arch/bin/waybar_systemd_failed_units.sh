#!/bin/bash

echo "SYSTEM FAILED UNITS:"
echo
systemctl --state=failed --no-pager
echo
echo
echo "USER FAILED UNITS:"
echo
systemctl --user --state=failed --no-pager
echo
