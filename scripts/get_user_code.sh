#!/bin/bash

. log

if [[ -z $1 ]]; then
	log "bad call to get_user_code.sh"
	exit 1
fi

user=$1

cd $user

