#!/bin/bash

LANG=C

echo -ne 'NCL '
date
echo -ne 'NOA '
TZ='America/Los_Angeles' date
echo -ne 'NOE '
TZ='Europe/Berlin' date
