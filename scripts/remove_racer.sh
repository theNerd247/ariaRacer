#!/bin/bash

if [[ -z $1 ]]; then
	echo "bad call to remove_user.sh" 1>&2
	exit 1
fi

user=$1

if [[ -d $user ]]; then
	rm -rf $user
	echo "removed user: $user" 1>&2
	exit 0
fi

log "$user already removed!"
