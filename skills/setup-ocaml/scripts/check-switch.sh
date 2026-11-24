#!/bin/sh
# check-switch.sh -- read-only OCaml/dune environment check.
#
# Usage: check-switch.sh [EXPECTED_SWITCH]
#
# Exits 0 only when opam, ocaml, and dune are visible and the selected switch
# matches (when one is named). It never installs, upgrades, or contacts the
# network.
set -eu

expected=${1:-}
fail=0

if ! command -v opam >/dev/null 2>&1; then
  echo "check-switch: opam not found" >&2
  exit 1
fi
if ! command -v ocaml >/dev/null 2>&1; then
  echo "check-switch: ocaml not found" >&2
  exit 1
fi
if ! command -v dune >/dev/null 2>&1; then
  echo "check-switch: dune not found" >&2
  exit 1
fi

switch=$(opam switch show 2>/dev/null || true)
ocaml_version=$(ocaml -version 2>/dev/null || true)
dune_version=$(dune --version 2>/dev/null || true)

case "$ocaml_version" in
  "The OCaml toplevel, version 5."*|"OCaml 5."*) : ;;
  *) echo "check-switch: expected OCaml 5, got: $ocaml_version" >&2; fail=1 ;;
esac

if [ -n "$expected" ] && [ "$switch" != "$expected" ]; then
  echo "check-switch: expected switch $expected, got ${switch:-none}" >&2
  fail=1
fi

if [ "$fail" -ne 0 ]; then
  exit 1
fi

echo "check-switch: OK switch=${switch:-none} ocaml=$ocaml_version dune=$dune_version"
