#!/bin/sh
# Offline success and failure fixtures for select-checks.sh.
set -eu
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
script="$here/../scripts/select-checks.sh"
tmp=$(mktemp -d "${TMPDIR:-/tmp}/verify-change-test.XXXXXX")
trap 'rm -rf -- "$tmp"' EXIT INT TERM

fail() { echo "FAIL: $*" >&2; exit 1; }

# Missing argument is a usage failure.
if "$script" >/dev/null 2>&1; then fail "missing args accepted"; fi
# Missing repository is a usage failure.
if "$script" "$tmp/nope" >/dev/null 2>&1; then fail "missing repo accepted"; fi

mkdir -p "$tmp/proj/tests"
: > "$tmp/proj/dune-project"
: > "$tmp/proj/Makefile"
out=$("$script" "$tmp/proj" src/a.ml)
printf '%s\n' "$out" | grep -q 'dune build' || fail "no dune suggestion"
printf '%s\n' "$out" | grep -q 'make -C' || fail "no make suggestion"
printf '%s\n' "$out" | grep -q 'pytest' || fail "no pytest suggestion"

mkdir -p "$tmp/sh"
: > "$tmp/sh/run.sh"
out=$("$script" "$tmp/sh" run.sh)
printf '%s\n' "$out" | grep -q 'sh -n' || fail "no shell parse suggestion"

# The script must not execute anything: a marker command must stay unrun.
mkdir -p "$tmp/evil"
printf 'touch %s/executed\n' "$tmp/evil" > "$tmp/evil/Makefile"
"$script" "$tmp/evil" >/dev/null
[ ! -e "$tmp/evil/executed" ] || fail "script executed a suggestion"

echo "verify-change script fixtures: PASS"
