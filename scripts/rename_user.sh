#!/bin/bash

. log

if (( $# != 2 )); then
	log "bad call to rename_user.sh: $@"
	exit 1
fi

log "renamin user $1 to $2"
remove_user.sh $1
create_user.sh $2
