#!/bin/bash

. log
. arserver.conf

if (( $# != 1 )); then
	log "bad new_user call: $@"
	exit 1
fi

user=$1

if [[ -d $user ]]; then
	log "user already exists: $user"
	exit 0
fi

log "creating user: $user"

set -x 
cp -r skel $user
cd $user
git init
git add -A
git commit -m "init repo"

log "user created: $user"
