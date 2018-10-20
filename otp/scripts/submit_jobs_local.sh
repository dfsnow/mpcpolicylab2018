#!/bin/bash

for x in 17031; do

    docker run -it --rm -e GEOID=$x otp

done
