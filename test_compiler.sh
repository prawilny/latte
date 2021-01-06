#!/bin/bash

set -e

BASEDIR=$(dirname "$0")
COMPILER=$BASEDIR/latc_x86_64
BLOB=$1

for FILE_PATH in $BLOB # good/*.lat
do
    FILE_PATH_STEM=${FILE_PATH%.lat}
    INPUT_FILE=$FILE_PATH_STEM.input

    echo $FILE_PATH_STEM:

    $COMPILER $FILE_PATH_STEM.lat

    touch $INPUT_FILE
    cat $INPUT_FILE | $BASEDIR/$FILE_PATH_STEM 1>$FILE_PATH_STEM.result

    diff $FILE_PATH_STEM.{output,result}
done
