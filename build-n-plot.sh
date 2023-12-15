#!/bin/bash

test_case_out="./dist/test_case_$1.out"
./build.sh $1 && $test_case_out && python plot.py data.csv
