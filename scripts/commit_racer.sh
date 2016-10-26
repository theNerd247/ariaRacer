#!/bin/bash

if [[ $# != 3 ]]; then
  echo "bad call to commit_racer.sh"
  exit 1
fi

user=$1
buildName=$2
outFile=$3

set -x -e

cd $user
git add ./ariaracer/src/main.cpp
git commit -m "create $buildName"
commits="$(git log | grep "commit" | cut -d ' ' -f 2)"

if (( $(echo "$commits" | wc -l) == 1 )); then
  exit 2
else
  echo "$(echo -n "$commits" | head -n1)" > $outFile
fi
