#!/bin/bash

if (( $# < 1 )); then
	echo "bad call to start_race.sh"
	exit 1
fi

bots=$@
nbots=$#
pidFile=/tmp/robPids
echo "" > $pidFile

set -xe
base=$(pwd)

for i in $(seq 1 $nbots); do
  cd $base
	racer=${@:$i:1}
	cd "$racer/build/ariaracer"
	./ariaracer -rh "robotIps[$i]" &
	echo $! >> $pidFile
done
