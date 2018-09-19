#!/usr/bin/env bash

retval=0
for test in imp_programs/*.imp; do
    output=$(./Cimp.native $test -d_ast 2>&1)
    success=$?
    mustfail=$(grep -E "\/\/.*MUSTFAIL" $test)
    passed=0

    # Must the compilation fail?
    if [ ! -z "$mustfail" ]; then
        [[ $success -ne 0 ]] && passed=1
    else
        [[ $success -eq 0 ]] && passed=1
    fi

    if [ $passed -eq 1 ]; then
        echo "$test: passed!"
    else
        echo "$test: failed!"
        printf "\t\t*****\n$output\n\t\t*****\n"
        retval=1
    fi

done

if [ $retval -eq 0 ]; then
  echo
  echo "All tests passed successfully!"
else
  echo
  echo "Some tests failed!"
fi

exit $retval
