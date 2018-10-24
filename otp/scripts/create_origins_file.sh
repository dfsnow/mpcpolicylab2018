#!/bin/bash

GEOID=17031
GEOMETRY='blocks'
CHUNKS=40

psql -d network -U snow -c "\COPY (
    SELECT geoid, ST_X(centroid) AS X, ST_Y(centroid) AS Y
    FROM $GEOMETRY
    WHERE ST_Contains((
        SELECT geom_buffer
        FROM counties
        WHERE geoid = '17031'),
        centroid)
    ) TO 'temp.csv' DELIMITER ',' CSV;"

echo "GEOID,Y,X" > $GEOID-origins.csv
awk -F, '{ print $1,$3,$2 }' OFS=, temp.csv >> $GEOID-origins.csv

if [ -n "$CHUNKS" ]; then
    tail -n +2 $GEOID-origins.csv > temp.csv
    rows=$(cat temp.csv | wc -l)
    chunk_size=$(expr $rows / $CHUNKS)
    split -l $chunk_size -d temp.csv x
    for chunk in $(seq -f "%02g" 1 $CHUNKS); do
        echo "GEOID,Y,X" > $GEOID-origins-$chunk.csv
        cat x$chunk >> $GEOID-origins-$chunk.csv
    done
fi

rm x*
rm temp.csv
