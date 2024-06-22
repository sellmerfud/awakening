
[private]
default:
  @just --list
  
# Show current version number
@showvers:
  grep '^\s*version' build.sbt

# Package up a new version
@release COMMIT="--commit" VERSION="next_minor":
  scripts/release.sh {{COMMIT}} {{VERSION}}



