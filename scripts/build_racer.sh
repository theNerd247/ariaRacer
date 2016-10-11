#!/bin/bash

. log

if (( $# != 2 )); then
	log "bad build_user call: $@"
	exit 1
fi

user=$1
sha=$2

if [[ ! -d $user ]]; then
	log "build_user user doesn't exist: $user"
	exit 2
fi

log "starting build for user $user for $sha"
set -x
set -e

if [[ ! -d $user ]]; then
	git clone "file://$gitserver/$user" $user
fi

cd $user

git reset --hard master
git pull origin master
git checkout $sha
mkdir -p build
cd build
rm -rf ./*
cmake ..
make
