#!/bin/bash

set -e

BASEDIR=$(dirname "$0")
COMPILER=$BASEDIR/latc_x86_64
BLOB=$1

for FILE_PATH in $BLOB # good/*.lat
do
    FILE_PATH_STEM=${FILE_PATH%.lat}
    if [ -f "$" ]; then
        INPUT_FILE=$FILE_PATH_STEM.input
    else
        INPUT_FILE="/dev/null"
    fi

    $COMPILER $FILE_PATH_STEM.lat

    $BASEDIR/$FILE_PATH_STEM < $INPUT_FILE > $FILE_PATH_STEM.result

    echo diff $FILE_PATH_STEM
    diff $FILE_PATH_STEM.{output,result}
done
