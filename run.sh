#!/usr/bin/env bash

# Should provide the directory where this script lives in most cases.
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

IMAGE_NAME="registry.gitlab.com/morrisanimalfoundation/grls-conditions-map"

# Build the image with our special build args.
# These matter more on Jenkins, but need to be placeheld anyway.
docker image build --no-cache -t $IMAGE_NAME --build-arg USER_ID=$(id -u ${USER}) .

 # Run the container in a disposable manner.
# Add a volume to the current working dir.
if ! command -v winpty &> /dev/null
then
  docker run --rm -it -v $SCRIPT_DIR:/workspace $IMAGE_NAME bash
else
  winpty docker run --rm -it -v $SCRIPT_DIR:/workspace $IMAGE_NAME bash
fi