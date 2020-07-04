#!/usr/bin/env bash

simple_put_response=$(fpm test simple_put)
expecte_simple_put_response="hello from simple_put"
if [[ "${simple_put_response}" != "${expecte_simple_put_response}" ]]; then
    echo "didn't get 'hello from simple_put'. got '${simple_put_response}'"
    exit 1
fi
