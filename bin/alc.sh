#!/bin/bash

if [ $# != 0 ]; then
    lynx -dump "http://eow.alc.co.jp/$*/UTF-8/?ref=sa" | less +38
else
    lynx -dump "http://www.alc.co.jp/"
fi
