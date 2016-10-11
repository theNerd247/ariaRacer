#!/bin/bash

if (( $# != 2 )); then
	echo "bad call to rename_user.sh: $@" 1>&2
	exit 1
fi

echo "renamin user $1 to $2" 1>&2
remove_user.sh $1
create_user.sh $2
