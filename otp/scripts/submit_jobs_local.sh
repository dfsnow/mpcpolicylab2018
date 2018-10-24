#!/bin/bash

WORKING_DIR='/resources/'
LOCATION_DIR='locations/'

for x in 17031; do

    docker run -it --rm \
        -v /home/snow/mpcpolicylab2018/otp/:$WORKING_DIR \
        -e WORKING_DIR=$WORKING_DIR \
        -e LOCATION_DIR=$LOCATION_DIR \
        -e MAX_THREADS=30 \
        -e CHUNKS=100 \
        -e GEOID=$x \
        otp

done
