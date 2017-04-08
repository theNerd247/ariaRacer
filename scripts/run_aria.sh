#!/bin/bash

# change this to the directory where aria.yml will be stored.
install="$(pwd)"

# determine if arrun and arrweb have been created. - use which
function getPath
{
  token="_$1"
  path=$(grep "$token" $install/aria.yaml | sed -e "s/^  $token: \(.*\)$/\1/")
  echo $path
}

scriptCwd=$(getPath "scriptCwd")

echo "We'll be executing scripts from $scriptCwd"

if test -z $scriptCwd; then
  echo "Could not parse script launch path in aria.yaml!"
  exit 1
fi

if ! test -d $scriptCwd; then
	mkdir $scriptCwd
fi

scriptBasePath=$(getPath "scriptBasePath")

echo "We'll be storing the script files in $scriptBasePath"

if test -z $scriptBasePath; then
  echo "Could not parse destination path for the scripts directory in aria.yaml"
  exit 1
fi

if test -d $scriptBasePath; then
  cp -r "$install/scripts/skel" "$scriptCwd/"
fi

if test -f /tmp/arrun_pid; then
  echo "arrun PID file already exists...aborting!"
  exit 2
fi

if test -f /tmp/arweb_pid; then
  echo "arweb PID file already exists...aborting!"
  exit 2
fi

# start the backend server 
arrun &
echo $! > /tmp/arrun_pid

# ...then start the web app...
arweb & 
echo $! > /tmp/arweb_pid
