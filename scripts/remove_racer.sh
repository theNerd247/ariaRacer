#!/bin/bash

if [[ -z $1 ]]; then
	echo "bad call to remove_user.sh"
	exit 1
fi

user=$1

set -e
set -x
rm -rf $user
echo "$user removed!"
