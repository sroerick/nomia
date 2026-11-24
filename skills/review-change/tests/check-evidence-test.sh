#!/bin/sh
# Offline success/failure fixtures for check-evidence.sh.
set -eu
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
script="$here/../scripts/check-evidence.sh"
tmp=$(mktemp -d "${TMPDIR:-/tmp}/review-change-test.XXXXXX")
trap 'rm -rf -- "$tmp"' EXIT INT TERM

fail() { echo "FAIL: $*" >&2; exit 1; }

good="$tmp/good.json"
cat > "$good" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 0}],
  "criteria": [{"id": "A1", "evidence": "test/test_yaml_lite.ml:42"}],
  "unrun": []
}
JSON
"$script" "$good" >/dev/null || fail "good evidence rejected"

bad="$tmp/bad.json"
cat > "$bad" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 1, "claimed_pass": true}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
if "$script" "$bad" >/dev/null 2>&1; then fail "nonzero claimed pass accepted"; fi

cat > "$tmp/empty.json" <<'JSON'
{"commands": [], "criteria": []}
JSON
if "$script" "$tmp/empty.json" >/dev/null 2>&1; then fail "empty evidence accepted"; fi

printf 'not json\n' > "$tmp/notjson"
if "$script" "$tmp/notjson" >/dev/null 2>&1; then fail "invalid json accepted"; fi

if "$script" >/dev/null 2>&1; then fail "missing argument accepted"; fi

echo "review-change script fixtures: PASS"
