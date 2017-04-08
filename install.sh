#!/bin/bash

prefix="$(pwd)/install"
install="$prefix/usr/share/ariaracer"
bin="$prefix/usr/bin"
installList="css fonts js scripts aria.yaml" 

mkdir -p $install

cp -v -r -u $installList $install

mkdir -p $bin
ln -s $install/scripts/run_aria.sh $bin/run_aria
ln -s $install/scripts/stop_aria.sh $bin/stop_aria
