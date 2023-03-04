#! /usr/bin/env bash

# This script will set the product version by modifying the appropriate source files
# Then use sbt to build and stage the files for the new version
# And finally zip up the results and copy the zip file to Dropbox
#
# usage:
# ./package.sh [<version>]

shopt -s extglob

usage() {
  {
    printf "usage: package.sh [--commit|--no_commit] [version]\n"
    printf "  --commit    - Commit changes and push them to Github (Default)\n"
    printf "  --no-commit - Do not commit changes\n"
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
    printf "\n$prompt (y/n) "
    read response
    case "$response" in
      y*|Y*) return 0;;
      n*|N*) return 1;;
      *) printf "Invalid response\n"
    esac
  done
}


# Update the version in all build.sbt, source files, and scripts
# Note we cannot update the README.md file until we have uploaded the
# zip file to Dropbox so that we can get its download URL. (see the update_readme() function)
set_version() {
  local version=$1
  
  ruby -p -i -e 'gsub(/(version\s*:=\s*)("\d+\.\d+")/, "\\1\"'$version'\"")' build.sbt
  ruby -p -i -e 'gsub(/'$jarfile_prefix'_2.13-(\d+\.\d+)\.jar/, "'$jarfile_prefix'_2.13-'$version'.jar")' src/other/$program_name src/other/$program_name.cmd
  ruby -p -i -e 'gsub(/(val\s+SOFTWARE_VERSION\s*=\s*)("\d+\.\d+")/, "\\1\"'$version'\"")' ${main_class}
  printf "Version set to $version\n"
}

create_package() {
  local version=$1

  PKG=$program_name-$version
  if [ -d target/$PKG ]; then
    find target/$PKG -name .DS_Store -exec rm {} \+
    rm -f target/${PKG}.zip
    (cd target; zip -rq ${PKG}.zip $PKG)
  else
    printf "Target directory: 'target/$PKG' does not exist\n"
    exit 1
  fi
}

# Add the files that we have modified to the git index,
# commit the release, and push it to Github
commit_release() {
  local version=$1

  git add  --update .
  git ci   -m"Update version number to $version"
  git tag  -m"Release v$version" v$version
  git push --tags origin master
}



# Get a short term access token for the dropbox api using our refresh token.
# We must do this because the access tokens are shot term and will expire
# in about 4 hours.
get_access_token() {
  local refresh_token="$(head -n1 ~/.dropbox/game_bots_refresh_token)"
  local client_id="$(head -n1 ~/.dropbox/game_bots_client_id)"
  local response=/tmp/access_token_response.$$
  local result=1

  curl -s https://api.dropbox.com/oauth2/token \
      -d grant_type=refresh_token \
      -d refresh_token="$refresh_token" \
      -d client_id="$client_id" > $response

  if fgrep --quiet '"error":' $response; then
    printf "Error getting access token\n" >&2
    jq . $response >&2
  else
    jq --raw-output .access_token $response
    result=0
  fi

  rm -f $response
  return $result
}

# Get the sharable url for the zip file and echo it to stdout
get_zipfile_url() {
  local version="$1"
  local dropbox_zip_file_path="/$program_name/$program_name-${version}.zip"
  local access_token=""
  local response=/tmp/get_zipfile_url_response.$$
  local result=1

  # NOTE:  We cannot assign this in the local variable declaration
  #        because we would lose the returned error code and would
  #        get the success error code from the 'local' function.
  access_token="$(get_access_token)"
  
  # If the url already exists then an error object is returned with the url buried
  # several layers down.  Otherwise it is in the field .url at top level.

  curl -s -X POST https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings \
      --header "Authorization: Bearer $access_token" \
      --header "Content-Type: application/json" \
      --data "{\"path\":\"${dropbox_zip_file_path}\"}" > $response

  if fgrep --quiet '"shared_link_already_exists":' $response; then
    jq --raw-output '.error.shared_link_already_exists.metadata.url' $response
    result=0
  elif fgrep --quiet '"error":' $response; then
    printf "Error getting zipfile url\n" >&2
    jq . $response >&2
  else
    jq --raw-output '.url' $response
    result=0
  fi
  
  rm -f $response
  return $result
}

upload_zipfile() {
  local version="$1"
  local local_zip_file_path="target/$program_name-${version}.zip"
  local dropbox_zip_file_path="/$program_name/$program_name-${version}.zip"
  local access_token=""
  local response=/tmp/upload_response.$$
  local result=1

  # NOTE:  We cannot assign this in the local variable declaration
  #        because we would lose the returned error code and would
  #        get the success error code from the 'local' function.
  access_token="$(get_access_token)"

  curl -s -X POST https://content.dropboxapi.com/2/files/upload \
      --header "Authorization: Bearer $access_token" \
      --header "Dropbox-API-Arg: {\"autorename\":false,\"mode\":\"overwrite\",\"mute\":false,\"path\":\"${dropbox_zip_file_path}\",\"strict_conflict\":false}" \
      --header "Content-Type: application/octet-stream" \
      --data-binary @"$local_zip_file_path"  >$response

  if fgrep --quiet '"error":' $response; then
    printf "Error uploading zip file\n" >&2
    jq . $response >&2
  else
    printf "$local_zip_file_path copied to Dropbox\n"
    result=0
  fi

  rm -f $response
  return $result
}

# Update the README.md file with the new
# version number and dropbox url
update_readme() {
  local version="$1"  
  local zip_file_url=""
  
  zip_file_url="$(get_zipfile_url $version)"
  
  ruby -p -i -e 'gsub(/\[Version\s*\d+\.\d+\]/, "[Version '$version']")' \
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
cd $(dirname $0)/..

ID_FILE="$(dirname $0)/local_package_vars.sh"
[[ -f "$ID_FILE" ]] || {
  printf "File not found: $ID_FILE\n"
  exit 1
}

# Sets the program_name and main_class variables
source $ID_FILE


[[ -z ${program_name+x} ]] && {
  printf "variable 'program_name' is not set in $ID_FILE\n"
  exit 1
}

[[ -z ${main_class+x} ]] && {
  printf "variable 'main_class' is not set in $ID_FILE\n"
  exit 1
}

[[ -z ${jarfile_prefix+x} ]] && {
  printf "variable 'jarfile_prefix' is not set in $ID_FILE\n"
  exit 1
}



# Make sure the main_class includes the .scala extentsion
main_class="${main_class%.scala}.scala"

# Make sure we are on the master branch
branch=$(git branch --show-current 2>/dev/null)

if [[ $? -ne 0 ]]; then
  printf "\Cannot determine the current branch!\n"
  exit 1
elif [[ $branch != "master" ]]; then
  printf "Must be on 'master' branch to create the package.\n"
  printf "Current branch is '$branch'"
  exit 1
  
fi

if repo_dirty; then
  printf "Working directory is not clean.\n"
  git status --short
  getYorN "Do you wish to continue anyway?" || exit 0
fi


CURRENT_VERSION=$(grep '^\s*version' build.sbt | tr '"' , | cut -d, -f2)

printf "\nCurrent version is $CURRENT_VERSION\n"
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
  printf "The current version does not have the correct format of <major.minor>\n"
  exit 1
fi

if [[ $CURRENT_VERSION != $NEW_VERSION ]]; then
  if getYorN "Set version to $NEW_VERSION and build package?"; then
    set_version $NEW_VERSION
  else
    exit 0
  fi
else
  getYorN "Build package for version $NEW_VERSION?" || exit 0
fi


set -euo pipefail
current_command=$BASH_COMMAND
# keep track of the last executed command
trap 'last_command=$current_command; current_command=$BASH_COMMAND' DEBUG
# echo an error message before exiting
trap 'printf "\"${last_command}\" command failed with exit code $?.\n"' EXIT

sbt stage
create_package $NEW_VERSION
upload_zipfile $NEW_VERSION
update_readme  $NEW_VERSION
if [[ $DO_COMMIT == yes ]]; then
  commit_release $NEW_VERSION
  printf "Version $NEW_VERSION successfully created and pushed to Github!"
else
  printf "Version $NEW_VERSION successfully created!"
fi

trap - DEBUG EXIT
