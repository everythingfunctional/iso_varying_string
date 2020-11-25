#!/usr/bin/env bash

set -e

FPM_FLAGS="--flag '-Wall' --flag '-Wextra' --flag '-Wimplicit-interface' --flag '-Werror' --flag '-fPIC' --flag '-g3' --flag '-fbounds-check' --flag '-fcheck-array-temporaries' --flag '-fbacktrace' --flag '-std=f2018' --flag '-fcheck=all'"
fpm build ${FPM_FLAGS}

simple_put_response=$(fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target simple_put)
expected_simple_put_response="hello from simple_put"
if [[ "${simple_put_response}" != "${expected_simple_put_response}" ]]; then
    echo "expected '${expected_simple_put_response}' but got '${simple_put_response}'"
    exit 1
fi

string_put_response=$(fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target string_put)
expected_string_put_response="hello from string_put"
if [[ "${string_put_response}" != "${expected_string_put_response}" ]]; then
    echo "expected '${expected_string_put_response}' but got '${string_put_response}'"
    exit 1
fi

fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target simple_get << EOF
hello simple_get
EOF

fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target get_maxlen << EOF
hello get_maxlen
EOF

fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target get_iostat << EOF
hello get_iostat
EOF

fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target get_terminator << EOF
hello,get terminator
EOF

simple_put_line_response=$(fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target simple_put_line)
expected_simple_put_line_response="hello from simple_put_line"
if [[ "${simple_put_line_response}" != "${expected_simple_put_line_response}" ]]; then
    echo "expected '${expected_simple_put_line_response}' but got '${simple_put_line_response}'"
    exit 1
fi

string_put_line_response=$(fpm test ${FPM_FLAGS} --runner "valgrind --leak-check=full --error-exitcode=1" --target string_put_line)
expected_string_put_line_response="hello from string_put_line"
if [[ "${string_put_line_response}" != "${expected_string_put_line_response}" ]]; then
    echo "expected '${expected_string_put_line_response}' but got '${string_put_line_response}'"
    exit 1
fi
