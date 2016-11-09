#!/bin/bash

if (( $# < 1 )); then
	echo "bad call to stop_race.sh"
	exit 1
fi

set -xe

nbots=$#
pidFile=/tmp/robPids

for i in $nbots; do
	racer=${@:$i:1}
	echo "killing lane $i"
	kill $(cat "$pidFile"_$racer)
done
