#!/bin/bash

psql -d network -U snow -c "\COPY (
    SELECT geoid, ST_X(centroid) AS X, ST_Y(centroid) AS Y
    FROM blocks
    WHERE ST_Contains((
        SELECT geom_buffer
        FROM counties
        WHERE geoid = '17031'),
        centroid)
    ) TO '17031-origins.csv' (format CSV);"
