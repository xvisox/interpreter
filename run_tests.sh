#!/bin/bash

echo "Building interpreter"
make > /dev/null 2>&1

test_files() {
    local dir_path=$1
    local expected_status=$2

    for file in "$dir_path"/in/*.see; do
        echo "Testing $file"

        ./interpreter "$file" > "$file.stdout" 2> "$file.stderr"
        local status=$?

        if [ $expected_status -eq 0 ] && [ $status -ne 0 ]; then
            echo "Test failed: Expected success but got failure"
            cat "$file.stderr"
            exit 1
        elif [ $expected_status -ne 0 ] && [ $status -eq 0 ]; then
            echo "Test failed: Expected failure but got success"
            cat "$file.stdout"
            exit 1
        fi

        local expected_file="${dir_path}/out/${file##*/}.out"
        local output_file="$file.stdout"

        if [[ $expected_status -ne 0 ]]; then
            output_file="$file.stderr"
        fi

        diff -u "$expected_file" "$output_file"
        if [ $? -ne 0 ]; then
            echo "Test failed: Output mismatch"
            diff "$expected_file" "$output_file"
            exit 1
        fi

        rm "$file.stdout" "$file.stderr"
    done
}

test_files "./examples/good" 0
test_files "./examples/complex" 0
test_files "./examples/bad" 1
test_files "./examples/typing" 1

echo "All tests passed."
