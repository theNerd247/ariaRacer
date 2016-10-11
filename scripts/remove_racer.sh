#!/bin/bash

. log

if [[ -z $1 ]]; then
	log "bad call to remove_user.sh"
	exit 1
fi

user=$1

if [[ -d $user ]]; then
	rm -rf $user
	log "removed user: $user"
	exit 0
fi

log "$user already removed!"
