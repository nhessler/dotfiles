#!/bin/sh
#/ Bootstrap machine with dependencies

set -e

abort() { STEP="";   echo "!!! $*" >&2; exit 1; }
log()   { STEP="$*"; echo "--> $*"; }
logn()  { STEP="$*"; printf -- "--> %s " "$*"; }
logk()  { STEP="";   echo "OK"; }

cleanup() {
  set +e

  if [-z "$SUCCESS"]; then
    if [ -n "$STEP" ]; then
      echo "!!! $STEP FAILED" >&2
    else
      echo "!!! UNKNOWN FAILURE" >&2
    fi
  fi
}

trap "cleanup" EXIT

SUCCESS="1"
logk
log "System Bootstrapped!"
