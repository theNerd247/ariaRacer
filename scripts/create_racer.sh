#!/bin/bash

if (( $# != 1 )); then
	echo "bad new_user call: $@" 1>&2
	exit 1
fi

user=$1

if [[ -d $user ]]; then
	echo "user already exists: $user" 1>&2
	exit 0
fi

echo "creating user: $user" 1>&2

set -e
set -x 
cp -r skel $user
cd $user
git init
git add -A
git commit -m "init repo"

echo "user created: $user" 1>&2
