#!/bin/bash

if (( $# < 1 )); then
	echo "bad call to start_race.sh"
	exit 1
fi

bots=$@
nbots=$#
echo "" > $pidFile

set -xe

for i in $nbots; do
	racer=${@:$i:1}
	cd "$racer/build/ariaracer"
	./ariaracer -rh "robotIps[$i]" &
	echo $! >> $pidFile
done
