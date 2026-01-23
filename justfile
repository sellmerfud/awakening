
[private]
default:
  @just --list --unsorted --justfile {{justfile()}}
  
# Show current version number
showvers:
  #! /usr/bin/env python3
  import re
  with open('build.sbt') as f:
    contents = f.read()
    match re.search(r'^\s*version\s*:=\s*"([^"]+)"', contents, re.MULTILINE):
      case None:
        print("Cannot determine current version!")
      case match:
        print(f"{match[1]}")

# Package up a new version
@release *ARGS:
  python3 scripts/release.py {{ARGS}}

# Build a release for Florian and copy it to Dropbox
to_florian:
  #! /usr/bin/env bash
  # VERS="6.0"
  VERS="$(grep -E 'version\s*:=' build.sbt | sed -e '/ *version/s/^ *version *:= *"\([^"]*\)".*$/\1/')"
  if test -n "$(git status --porcelain)"; then
    echo "Working directory is not clean!"
    exit
  else
    sbt stage
    rm -f target/awakening-"$VERS"/commit_*
    COMMIT=$(cat .git/refs/heads/florian | head -c 10)
    COMMIT_FILE="awakening-"$VERS"/commit_$COMMIT"
    echo "$COMMIT" > target/"$COMMIT_FILE"
    (cd target; zip awakening-"$VERS".zip "$COMMIT_FILE")
    cp target/awakening-"$VERS".zip ~/Dropbox/Public/
  fi

# Dumps the contents of a log-nnn file
[no-cd]
@dump_log path:
  jq -r '.log[].text' '{{path}}'

# Shows the software-version, and file-version of a save-nnn file
[no-cd]
game_info path:
  #! /usr/bin/env bash
   jq '{ "software-version",
         "file-version",
         "botEnhancements": ."game-state"."botEnhancements",
         "scenarioName": ."game-state"."scenarioName",
         "# turn actions": ."game-state"."turnActions" | length
       }' '{{path}}'

@saved_game path:
  mkdir -p games && cd games && ouch decompress '{{path}}'
