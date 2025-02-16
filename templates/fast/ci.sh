#! /bin/env sh

# Run the fast CI suite

# Get the build id from the first argument
PROJECT_ID=$1
BUILD_ID=$2

echo "Running fast CI suite in build $BUILD_ID"

# Build the docker image
docker build -t $PROJECT_ID-fast-ci-suite .
# Run the docker container and inject the build id
docker run -it $PROJECT_ID-fast-ci-suite $BUILD_ID
