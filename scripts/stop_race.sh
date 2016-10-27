#!/bin/bash

if (( $# < 1 )); then
	echo "bad call to stop_race.sh"
	exit 1
fi

set -xe

for i in $nbots; do
	kill $(head -n ${@:$i:1} $pidFile | tail -1)
done
