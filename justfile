
[private]
default:
  @just --list --list-prefix=' - ' --list-heading=$'' --unsorted --justfile {{justfile()}}
  
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
  vers="$(grep -E 'version\s*:=' build.sbt)"
  pattern='[[:space:]]*version[[:space:]]*:=[[:space:]]"([^"]*)"'
  [[ $vers =~ $pattern ]] && vers="${BASH_REMATCH[1]}"
  exit
  if test -n "$(git status --porcelain)"; then
    echo "Working directory is not clean!"
    exit
  else
    sbt stage
    rm -f target/awakening-"$vers"/commit_*
    commit=$(cat .git/refs/heads/florian | head -c 10)
    commit_file="awakening-"$vers"/commit_$commit"
    echo "$commit" > target/"$commit_file"
    (cd target; zip awakening-"$vers".zip "$commit_file")
    cp target/awakening-"$vers".zip ~/Dropbox/Public/
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

[private]
@make_games_dir:
  mkdir -p games

[working-directory: 'games']
@extract_game path: make_games_dir
  ouch decompress '{{path}}'
