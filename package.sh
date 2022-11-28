#! /usr/bin/env bash

# This script will set the product version by modifying the appropriate source files
# Then use sbt to build and stage the files for the new version
# And finally zip up the results and copy the zip file to our ~/Dropbox/fitl directory
#
# usage:
# ./package.sh [<version>]

usage() {
  echo "usage: package.sh [version]" 1>&2
  exit 1
}

BUILD_DIR=/Users/curt/dev/projects/awakening

# In order for sbt to work correctly we must be run from the top level fitl directory
validate_cwd() {
  [[ $(pwd) == "$BUILD_DIR" ]] && return
  echo "This script must be run with the current working directory equal to $BUILD_DIR"
  exit 1
}

validate_version() {
  echo "$1" | egrep '\d+\.\d+' >/dev/null 2>&1 && return
  echo "The version parameter must be in the form: major.minor"
  echo "Where major and minor are integers"
  exit 1
}


validate_cwd

test $# -gt 1 && usage



CURRENT_VERSION=$(grep '^\s*version' build.sbt | tr '"' , | cut -d, -f2)

if [[ $# -eq 1 ]]; then
  NEW_VERSION=$1
  validate_version $NEW_VERSION
  if [[ $CURRENT_VERSION != $NEW_VERSION ]]; then
    echo "Setting version to $NEW_VERSION"
    just setvers $NEW_VERSION
  else
    echo "Current version is $NEW_VERSION"
  fi
else
  NEW_VERSION=$CURRENT_VERSION
  echo "Current version is $NEW_VERSION"
fi

set -e
# keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

sbt stage
just package $NEW_VERSION

trap - DEBUG EXIT