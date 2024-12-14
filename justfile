
[private]
default:
  @just --list --justfile {{justfile()}}
  
# Show current version number
@showvers:
  grep '^\s*version' build.sbt

# Package up a new version
@release *ARGS:
  scripts/release.sh {{ARGS}}

# Build a release for Florian and copy it to Dropbox
to_florian:
  #! /usr/bin/env bash
  if test -n "$(git status --porcelain)"; then
    echo "Working directory is not clean!"
    exit
  else
    sbt stage
    rm target/awakening-5.6-florian/commit_*
    COMMIT=$(cat .git/refs/heads/florian)
    COMMIT_FILE="target/awakening-5.6-florian/commit_$COMMIT"
    echo "$COMMIT" > "$COMMIT_FILE"
    zip -j target/awakening-5.6-florian.zip "$COMMIT_FILE"
    cp target/awakening-5.6-florian.zip ~/Dropbox/Public/
  fi

