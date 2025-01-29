
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
  VERS="6.0"
  if test -n "$(git status --porcelain)"; then
    echo "Working directory is not clean!"
    exit
  else
    sbt stage
    rm -f target/awakening-"$VERS"/commit_*
    COMMIT=$(cat .git/refs/heads/florian | head -c 10)
    COMMIT_FILE="target/awakening-"$VERS"/commit_$COMMIT"
    echo "$COMMIT" > "$COMMIT_FILE"
    zip -j target/awakening-"$VERS".zip "$COMMIT_FILE"
    cp target/awakening-"$VERS".zip ~/Dropbox/Public/
  fi

# Dumps the contents of a log-nnn file
[no-cd]
@dump_log path:
  jq -r '.log[].text' '{{path}}'

# Shows the software-version, anbd file-version of a save-nnn file
[no-cd]
save_info path:
  #! /usr/bin/env bash
   jq '{ "software-version",
         "file-version",
         "botEnhancements": ."game-state"."botEnhancements",
         "scenarioName": ."game-state"."scenarioName",
         "# turn actions": ."game-state"."turnActions" | length
       }' '{{path}}'

@saved_game path:
  ouch decompress -d games '{{path}}'
