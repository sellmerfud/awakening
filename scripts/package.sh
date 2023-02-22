#! /usr/bin/env bash

# This script will set the product version by modifying the appropriate source files
# Then use sbt to build and stage the files for the new version
# And finally zip up the results and copy the zip file to our ~/Dropbox/fitl directory
#
# usage:
# ./package.sh [<version>]

shopt -s extglob

usage() {
  echo "usage: package.sh [<major.mingor>|next_major|next_minor]" 1>&2
  echo "       where: major and minor are integers" 1>&2
  exit 1
}

BUILD_DIR=/Users/curt/dev/projects/fitl

repo_dirty() {
  test -n "$(git status --porcelain)"
}

getYorN() {
  local prompt="$1"
  local response
  
  while true; do
    echo -en "\n$prompt (y/n) "
    read response
    case "$response" in
      y*|Y*) return 0;;
      n*|N*) return 1;;
      *) echo "Invalid resopnse"
    esac
  done
}

case "$1" in
  "") 
    NEW_VERSION=current
    ;;
    
  +([0-9]).+([0-9]))
    NEW_VERSION="$1"
    ;;
  
  next_minor)
    NEW_VERSION=next_minor
    ;;
  
  next_major)
    NEW_VERSION=next_major
    ;;

  *)
    usage
    ;;
esac

## Set the current working directory to the directory where this script is running.
## (The top level working directory of the git repository)
## This is important because sbt' must be run from the top level directory
cd $(dirname $0)

CURRENT_VERSION=$(grep '^\s*version' build.sbt | tr '"' , | cut -d, -f2)

echo "Current version is $CURRENT_VERSION"
if [[ $CURRENT_VERSION =~ ^([[:digit:]]+)\.([[:digit:]]+)$ ]]; then
  MAJOR=${BASH_REMATCH[1]}
  MINOR=${BASH_REMATCH[2]}
  
  case $NEW_VERSION in
    current   ) NEW_VERSION=$CURRENT_VERSION ;;
    next_major) NEW_VERSION=$(($MAJOR + 1)).0 ;;
    next_minor) NEW_VERSION=$MAJOR.$(($MINOR + 1)) ;;
    *         ) ;; # NEW_VERSION was explicitly given as the argument
  esac
else
  echo "The current version does not have the correct format of <major.minor>"
  exit 1
fi

if repo_dirty; then
  echo -e "\nWorking directory is not clean."
  git status --short
  getYorN "Do you wish to continue anyway?" || exit 1
fi

if [[ $CURRENT_VERSION != $NEW_VERSION ]]; then
  if getYorN "Set version to $NEW_VERSION and build package?"; then
    just setvers $NEW_VERSION
  else
    exit 0
  fi
else
  getYorN "Build package for version $NEW_VERSION?" || exit 0
fi


set -e
# keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# echo an error message before exiting
trap 'echo "\"${last_command}\" command failed with exit code $?."' EXIT

sbt stage
just package $NEW_VERSION

trap - DEBUG EXIT