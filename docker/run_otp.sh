#!/bin/bash

# Create the OTP graph object
java -jar /otp/otp-$OTP_VERSION-shaded.jar \
    --cache /otp/ \
    --basePath /otp/ \
    --build /otp/graphs/$GEOID

# Create the OTP matrix
java -jar /otp/jython-standalone-$JYTHON_VERSION.jar \
    -Dpython.path=/otp/otp-$OTP_VERSION-shaded.jar \
    /otp/create_otp_matrix.py