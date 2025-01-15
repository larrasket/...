#!/usr/bin/env sh

output="$(emacsclient -e "(let ((inhibit-message t)) (salih/get-mail-password))")"

output_without_quotes="${output//\"/}"

echo "$output_without_quotes"
