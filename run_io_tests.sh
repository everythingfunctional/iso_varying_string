#!/usr/bin/env bash

set -e

fpm build

simple_put_response=$(fpm test simple_put)
expected_simple_put_response="hello from simple_put"
if [[ "${simple_put_response}" != "${expected_simple_put_response}" ]]; then
    echo "expected '${expected_simple_put_response}' but got '${simple_put_response}'"
    exit 1
fi

string_put_response=$(fpm test string_put)
expected_string_put_response="hello from string_put"
if [[ "${string_put_response}" != "${expected_string_put_response}" ]]; then
    echo "expected '${expected_string_put_response}' but got '${string_put_response}'"
    exit 1
fi

fpm test simple_get << EOF
hello simple_get
EOF

fpm test get_maxlen << EOF
hello get_maxlen
EOF

fpm test get_iostat << EOF
hello get_iostat
EOF
