

_default:
  @just --list
  
# Create zip file and copy it to dropbox
package VERSION:
  #!/usr/bin/env bash
  set -euo pipefail
  PKG=awakening-{{VERSION}}
  if [ -d target/$PKG ]; then
    find target/$PKG -name .DS_Store -exec rm {} \+
    rm -f target/${PKG}.zip
    (cd target; zip -rq ${PKG}.zip $PKG)
    cp target/${PKG}.zip /Users/curt/Dropbox/awakening/
  else
    echo target/$PKG does not exist
    exit 1
  fi

@setvers VERSION:
  ruby -p -i -e 'gsub(/(version\s*:=\s*)("\d+\.\d+")/, "\\1\"{{VERSION}}\"")' build.sbt
  ruby -p -i -e 'gsub(/awakening_2.11-(\d+\.\d+)\.jar/, "awakening_2.11-{{VERSION}}.jar")' src/other/awakening src/other/awakening.cmd

