#!/bin/bash

if (( $# < 1 )); then
	echo "bad call to start_race.sh"
	exit 1
fi

bots=$@
nbots=$#
pidFile="/tmp/robPids"
robSubnet="10.0.126"

robotIps[0]="foo"
robotIps[1]="$robSubnet.13"
robotIps[2]="$robSubnet.11"
echo "" > $pidFile

#set -xe
base=$(pwd)

for i in $(seq 1 $nbots); do
  cd $base
	racer=${@:$i:1}
	cd "$racer/build/ariaracer"
  echo "running ${robotIps[$i]}"
	nohup ./ariaracer -rh "${robotIps[$i]}" > /tmp/race_log_$i.txt &
	pid=$!
	pidF="$pidFile"_$i
	echo "pid $pid"
	echo $pid >> $pidF
	disown $pid
done
