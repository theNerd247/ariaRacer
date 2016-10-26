#!/bin/bash

if (( $# > 2 || $# < 1 )); then
	echo "bad build_racer.sh call: $@" 1>&2
	exit 1
fi

user=$1
sha=$2

if [[ ! -d $user ]]; then
	echo "build_user user doesn't exist: $user" 1>&2
	exit 2
fi

echo "starting build for user $user for $sha" 1>&2
set -x
set -e

cd $user

if [[ $2 ]]; then
  git reset --hard HEAD
  git checkout $sha
fi

mkdir -p build
cd build
rm -rf ./*
cmake ..

set +x

make 2>&1 
