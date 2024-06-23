#! /usr/bin/env bash

# This script will set the product version by modifying the appropriate source files
# Then use sbt to build and stage the files for the new version
# And finally zip up the results and copy the zip file to Dropbox
#

shopt -s extglob

usage() {
  {
    printf "usage: package.sh [--commit|--no_commit|-n] [version]\n"
    printf "  --commit    - Commit changes and push them to Github (Default)\n"
    printf "  --no-commit - Do not commit changes\n"
    printf "  -n          - Do not commit changes (same as --no-commit)\n"
    printf "\n"
    printf "  version     - Can be one of:\n"
    printf "                next_minor: Bump the minor version number (Default)\n"
    printf "                next_major: Bump the major version number and set minor to zero\n"
    printf "                <major>.<minor>: where: major and minor are integers\n"
  } 1>&2
  exit 1
}

repo_dirty() {
  test -n "$(git status --porcelain)"
}

getYorN() {
  local prompt="$1"
  local response
  
  while true; do
    printf "\n%s (y/n) " "$prompt"
    read -r response
    case "$response" in
      y*|Y*) return 0;;
      n*|N*) return 1;;
      *) printf "Invalid response\n"
    esac
  done
}


# Update the version in build.sbt
# Note we cannot update the README.md file until we have uploaded the
# zip file to Dropbox so that we can get its download URL. (see the update_readme() function)
set_version() {
  local version=$1
  
  ruby -p -i -e 'gsub(/(version\s*:=\s*)("\d+\.\d+")/, "\\1\"'"$version"'\"")' build.sbt
  printf "Version set to %s\n" "$version"
}

zipfile_name() {
  local version=$1
  printf "%s" "$program_name-${version}.zip"
}

# Add the files that we have modified to the git index,
# commit the release, and push it to Github
commit_release() {
  local version=$1
  local version_label="v$version"
  local local_zip_file_path

  local_zip_file_path="target/$(zipfile_name "$version")"

  git add  --update .
  git ci   -m"build: update version number to $version"
  git tag  -m"Release $version_label" "$version_label"
  git push --tags origin master
    # Create the release and upload the zip file to the release assests
  gh release create \
    --generate-notes \
    --title "Version $version" \
    "$version_label" \
    "$local_zip_file_path"
}



# Update the README.md file with the new
# version number and dropbox url
update_readme() {
  local version="$1"  
  local zip_file_url=""
  
  zip_file_url="https://github.com/sellmerfud/${repo_name}/releases/download/v${version}/$(zipfile_name "$version")"
  
  ruby -p -i -e 'gsub(/\[Version\s*\d+\.\d+\]/, "[Version '"$version"']")' \
             -e 'gsub(/^\[1\]:.*$/, "[1]: '"$zip_file_url"'")' README.md
}

# Start of main script

# Commit changes by default
DO_COMMIT=yes

case "$1" in
  --commit)
    shift
    ;;
    
  -n|--no-commit)
    DO_COMMIT=no
    shift
    ;;
    
  -*)
    usage
    ;;
esac

# The deafault action if no paramter is given is to update the minor version number
case "$1" in
  "") 
    NEW_VERSION=next_minor
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

  
## Set the current working directory to the parent directory of this script.
## (The top level working directory of the git repository)
## This is important because sbt' must be run from the top level directory
cd "$(dirname "$0")"/..

# Program name and dropbox folder are used to
# upload the zip file to dropbox
repo_name=awakening
program_name=awakening

# Make sure we are on the master branch
branch=$(git branch --show-current 2>/dev/null)

if ! branch=$(git branch --show-current 2>/dev/null); then
  printf "\Cannot determine the current branch!\n"
  exit 1
elif [[ $branch != "master" ]]; then
  printf "Must be on 'master' branch to create the release.\n"
  printf "Current branch is '%s'\n" "$branch"
  exit 1
  
fi

if repo_dirty; then
  printf "Working directory is not clean.\n"
  git status --short
  getYorN "Do you wish to continue anyway?" || exit 0
fi


CURRENT_VERSION=$(grep '^\s*version' build.sbt | tr '"' , | cut -d, -f2)

printf "\nCurrent version is %s\n" "$CURRENT_VERSION"
if [[ $CURRENT_VERSION =~ ^([[:digit:]]+)\.([[:digit:]]+)$ ]]; then
  MAJOR=${BASH_REMATCH[1]}
  MINOR=${BASH_REMATCH[2]}
  
  case $NEW_VERSION in
    current   ) NEW_VERSION=$CURRENT_VERSION ;;
    next_major) NEW_VERSION=$((MAJOR + 1)).0 ;;
    next_minor) NEW_VERSION=$MAJOR.$((MINOR + 1)) ;;
    *         ) ;; # NEW_VERSION was explicitly given as the argument
  esac
else
  printf "The current version does not have the correct format of <major.minor>\n"
  exit 1
fi

if [[ $CURRENT_VERSION != "$NEW_VERSION" ]]; then
  if getYorN "Set version to $NEW_VERSION and create a release?"; then
    set_version "$NEW_VERSION"
  else
    exit 0
  fi
else
  getYorN "Create a release for version $NEW_VERSION?" || exit 0
fi


set -euo pipefail
current_command=$BASH_COMMAND
# keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# echo an error message before exiting
trap 'printf "\"${last_command}\" command failed with exit code $?.\n"' EXIT

sbt stage
update_readme  "$NEW_VERSION"
if [[ $DO_COMMIT == yes ]]; then
  commit_release "$NEW_VERSION"
  printf "Version %s successfully created and pushed to Github!" "$NEW_VERSION"
else
  printf "Version %s successfully created!" "$NEW_VERSION"
fi

trap - DEBUG EXIT
