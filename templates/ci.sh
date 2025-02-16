#!/usr/bin/env sh

# This script is used to run the CI tests.
# It is called by the git commit hook on the developer system.
# It could also be called by a CI server.

# The job of this script is to:
# - Assign a build id
# - Execute fast/ci.sh with the build id
# - Execute slow/ci.sh with the build id

# Generate a random 10-character Base58 string
generate_base58_id() {
    # Base58 characters (Bitcoin alphabet)
    B58_CHARS="123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    
    # Initialize empty string
    id=""
    
    # Generate 10 random characters
    for i in $(seq 1 10); do
        # Get a random byte and reject values that would create modulo bias
        while true; do
            rand=$(od -An -N1 -i /dev/urandom | tr -d ' ')
            # Only use the byte if it's less than the largest multiple of 58 <= 255
            # 232 is the largest multiple of 58 less than 256 (58 * 4 = 232)
            [ "$rand" -lt 232 ] && break
        done
        index=$((rand % 58))
        # Append character to id
        id="${id}${B58_CHARS:index:1}"
    done
    
    echo "$id"
}

PROJECT_ID="{{project_id}}"
BUILD_ID=$(generate_base58_id)

# Call fast/ci.sh with the build id
fast/ci.sh $PROJECT_ID $BUILD_ID

# Call slow/ci.sh with the build id
slow/ci.sh $PROJECT_ID $BUILD_ID










