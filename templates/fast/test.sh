#! /bin/env sh

# This script is run inside the docker container

# Simulate a test failure
echo "This is a test failure" >&2
exit 1
