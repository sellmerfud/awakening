

_default:
  @just --list
  
# Show current version number
@showvers:
  grep '^\s*version' build.sbt

# Package up a new version
package COMMIT="--commit" VERSION="next_minor":
  scripts/package.sh {{COMMIT}} {{VERSION}}
