#!/bin/bash

if (( $# != 4 )); then
  echo "bad call to upload_code.sh"
  exit 1
fi

user=$1
file=$2
buildName=$3
newFile=$(basename $file)
outFile=$4

set -x -e

cd $user
scp $file ./$newFile
git add $newFile
git commit -m "create $buildName with $newFile"
commits="$(git log | grep "commit" | cut -d ' ' -f 2)"

if (( $(echo "$commits" | wc -l) == 1 )); then
  exit 2
else
  echo "$(echo -n "$commits" | head -n1)" > $outFile
fi
