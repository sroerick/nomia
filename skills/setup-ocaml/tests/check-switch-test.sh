#!/bin/sh
# Offline success/failure fixtures for check-switch.sh using PATH stubs.
set -eu
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
script="$here/../scripts/check-switch.sh"
tmp=$(mktemp -d "${TMPDIR:-/tmp}/setup-ocaml-test.XXXXXX")
trap 'rm -rf -- "$tmp"' EXIT INT TERM

fail() { echo "FAIL: $*" >&2; exit 1; }

stub() { # dir switch ocaml dune
  mkdir -p "$1"
  printf '#!/bin/sh\necho %s\n' "$2" > "$1/opam"
  printf '#!/bin/sh\necho "%s"\n' "$3" > "$1/ocaml"
  printf '#!/bin/sh\necho %s\n' "$4" > "$1/dune"
  chmod +x "$1/opam" "$1/ocaml" "$1/dune"
}

stub "$tmp/goodbin" poohstack "The OCaml toplevel, version 5.4.1" 3.23.0
PATH="$tmp/goodbin:$PATH" "$script" poohstack >/dev/null || fail "good environment rejected"

if PATH="$tmp/goodbin:$PATH" "$script" other >/dev/null 2>&1; then
  fail "wrong switch accepted"
fi

stub "$tmp/oldbin" poohstack "The OCaml toplevel, version 4.14.0" 3.23.0
if PATH="$tmp/oldbin:$PATH" "$script" poohstack >/dev/null 2>&1; then
  fail "OCaml 4 accepted"
fi

mkdir -p "$tmp/emptybin"
if PATH="$tmp/emptybin:/usr/bin:/bin" "$script" >/dev/null 2>&1; then
  fail "missing tools accepted"
fi

echo "setup-ocaml script fixtures: PASS"
