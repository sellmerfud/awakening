
[private]
default:
  @just --list --justfile {{justfile()}}
  
# Show current version number
@showvers:
  grep '^\s*version' build.sbt

# Package up a new version
@release *ARGS:
  scripts/release.sh {{ARGS}}




