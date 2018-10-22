#!/bin/bash

WORKING_DIR='/resources/'

for x in 17031; do

    docker run -it --rm \
        -v /home/snow/mpcpolicylab2018/otp/:$WORKING_DIR \
        -e WORKING_DIR=$WORKING_DIR \
        -e GEOID=$x \
        otp

done
