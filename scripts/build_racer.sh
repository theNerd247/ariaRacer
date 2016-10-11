#!/bin/bash

if (( $# != 2 )); then
	echo "bad build_user call: $@" 1>&2
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

if [[ ! -d $user ]]; then
	git clone "file://$gitserver/$user" $user
fi

cd $user

git reset --hard master
git checkout $sha
mkdir -p build
cd build
rm -rf ./*
cmake ..
make
