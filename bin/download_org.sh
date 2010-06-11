#!/bin/bash

command() {
    sleep 1; echo '20489314'
    sleep 2; echo 'mitsu1523'
    sleep 2; echo 'mget *'
}

cd /home/shimo/org/stage/
rm -rf *
command | cadaver http://jellybelly.ddo.jp/share/org/
