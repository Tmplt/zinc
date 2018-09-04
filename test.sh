#!/usr/bin/env bash

for test in imp_programs/*.imp; do
    output=$(./Cimp.native $test -d_ast 2>&1)
    [[ $? -eq 0 ]] && echo "$test: passed!" || printf "\t\t*****\n$output\n\t\t*****\n"
done
