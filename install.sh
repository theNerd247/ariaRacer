#!/bin/bash

prefix="install"
install="$prefix/usr/share/ariaracer"
installList="css fonts js scripts" 

mkdir -p $install

cp -v -r -u $installList $install
