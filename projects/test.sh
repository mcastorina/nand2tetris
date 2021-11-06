#!/bin/bash

# path to HardwareSimulator.sh
hw_sim=/usr/bin/n2tHardwareSimulator

if [[ -z $1 ]]; then
    echo "please provide a directory containing *.tst files"
    exit 1
fi

cd "$1"
for file in *.tst; do
    printf "%-20s" "${file%.tst}"
    if $hw_sim $PWD/$file >/dev/null 2>&1; then
        echo -e '\e[0;32mok\e[39;49m'
    else
        echo -e '\e[0;31mfail\e[39;49m'
    fi
done
