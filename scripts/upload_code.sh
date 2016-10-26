#!/bin/bash

if (( $# != 2 )); then
  echo "bad call to upload_code.sh" 1>&2
  exit 1
fi

user=$1
file=$2

set -x -e

cd $user
git reset --hard master
scp $file ./ariaracer/src/main.cpp
