#!/bin/bash

make > /dev/null 2>&1

for file in ./examples/good/*.see; do
    echo "Testing $file"
    ./interpreter $file > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        echo "Test failed"
        exit 1
    fi
done
