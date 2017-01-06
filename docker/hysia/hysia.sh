#!/bin/sh

/usr/local/bin/hss.opt $@
if [ -e pped.dat ]; then
    echo "\ndumped data:\n"
    cat pped.dat
    rm pped.dat
fi
