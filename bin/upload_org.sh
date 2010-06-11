#!/bin/bash

command() {
    sleep 1; echo '20489314'
    sleep 2; echo 'mitsu1523'
    sleep 2; echo 'mput *'
}

cd /home/shimo/org/stage/
command | cadaver http://jellybelly.ddo.jp/share/org/
