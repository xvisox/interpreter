#!/bin/bash

cd ./lang

bnfc -m seeemcrd.cf --functor

make

cd ../examples/good

for file in *.see; do
    cat "$file" | ../../lang/TestSeeemcrd > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        echo "File parsed successfully: $file"
    else
        echo "Failed to parse $file."
    fi
done
