#!/bin/bash

psql -d network -U snow -c "\COPY (
    SELECT geoid, ST_X(centroid) AS X, ST_Y(centroid) AS Y
    FROM tracts
    WHERE ST_Contains((
        SELECT geom_buffer
        FROM counties
        WHERE geoid = '17031'),
        centroid)
    ) TO 'temp.csv' DELIMITER ',' CSV;"

echo "GEOID,Y,X" > $GEOID-origins.csv
awk -F, '{ print $1,$3,$2 }' OFS=, temp.csv >> $GEOID-origins.csv

rm temp.csv
