#!/bin/sh
# select-checks.sh -- deterministic, offline check suggestions.
#
# Usage: select-checks.sh REPO [PATH...]
#
# Prints one suggested command per line. It never executes anything, contacts
# the network, reads credentials, or writes to the repository. The caller owns
# the decision and the evidence.
set -eu

if [ "$#" -lt 1 ]; then
  echo "usage: select-checks.sh REPO [PATH...]" >&2
  exit 2
fi

repo=$1
shift

if [ ! -d "$repo" ]; then
  echo "select-checks: not a directory: $repo" >&2
  exit 2
fi

# Refuse an implicit whole-home or whole-filesystem scan.
case "$(cd "$repo" && pwd)" in
  /|"$HOME") echo "select-checks: refusing implicit scope: $repo" >&2; exit 2 ;;
esac

emit() { printf '%s\n' "$1"; }

emit "git -C $repo diff --check"

if [ -f "$repo/dune-project" ]; then
  emit "dune build --root $repo @all"
  emit "dune runtest --root $repo --force"
fi
if [ -f "$repo/Makefile" ]; then
  emit "make -C $repo test"
fi
if [ -f "$repo/go.mod" ]; then
  emit "go -C $repo test ./..."
fi
if [ -f "$repo/Cargo.toml" ]; then
  emit "cargo test --manifest-path $repo/Cargo.toml"
fi
if [ -f "$repo/pyproject.toml" ] || [ -f "$repo/setup.cfg" ] || [ -d "$repo/tests" ]; then
  emit "python3 -m pytest $repo"
fi
if [ -f "$repo/package.json" ]; then
  if grep -q '"test"' "$repo/package.json" 2>/dev/null; then
    emit "npm --prefix $repo test"
  fi
fi

# Shell changes get a parse check even when no project marker exists.
for p in "$@"; do
  case "$p" in
    *.sh) emit "sh -n $repo/$p" ;;
  esac
done

if [ "$#" -eq 0 ]; then
  echo "select-checks: no changed paths supplied; suggestions only" >&2
fi
