#!/bin/bash

kill $(cat /tmp/arrun_pid)
rm /tmp/arrun_pid

kill $(cat /tmp/arweb_pid)
rm /tmp/arweb_pid
