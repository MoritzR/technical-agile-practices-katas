#!/bin/bash

echo "------ Running Golden Master Test ------"
expected=$(cat expectedOutput.txt)
actual=$(stack run --silent)

diff=$(diff <(echo "$expected") <(echo "$actual"))

if [ "$diff" != "" ] 
then
    if command -v colordiff;
    then
        diff <(echo "$expected") <(echo "$actual") | colordiff
    else
        diff <(echo "$expected") <(echo "$actual")
    fi
    echo "Test Failure"
else
    echo "Test Success"
fi