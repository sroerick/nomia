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

# -- receipt mode (borg/gate-receipts.borg, G2) ------------------------------
# claims.sh verify is stubbed: the offline fixtures never touch a server.

stub="$tmp/claims-stub.sh"
cat > "$stub" <<'SH'
#!/bin/sh
# $1 = verify, $2 = claim path (a fixture name here)
case "$2" in
  *unresolvable*) echo "claims.sh: cannot read $2 (missing, or no covering grant for it)" >&2; exit 2 ;;
  *mismatch*)     echo "MISMATCH the cited computation does not reproduce"; exit 1 ;;
  *pass*)         echo "VERIFIED result hash, step count and status agree"; echo "verdict=pass"; exit 0 ;;
  *fail*)         echo "VERIFIED result hash, step count and status agree"; echo "verdict=fail"; exit 0 ;;
  *unverified*)   echo "VERIFIED result hash, step count and status agree"; echo "verdict=unverified"; exit 0 ;;
  *)              echo "VERIFIED result hash, step count and status agree"; exit 0 ;;
esac
SH
chmod +x "$stub"

receipts() { CLAIMS_SH="$stub" "$script" --receipts "$@"; }

# agreeing receipts: a pass with exit 0, a fail with exit 1
cat > "$tmp/r-ok.json" <<'JSON'
{
  "commands": [
    {"command": "dune runtest nomia", "exit": 0, "receipt": "u.r.claim.2026-11-24.gate.tuna.abcdefabcdef.pass"},
    {"command": "make test-broken", "exit": 1, "receipt": "u.r.claim.2026-11-24.gate.tuna.abcdefabcdef.fail"}
  ],
  "criteria": [{"id": "A1", "evidence": "test/test_yaml_lite.ml:42"}]
}
JSON
receipts "$tmp/r-ok.json" >/dev/null || fail "agreeing receipts rejected"

# disagreeing receipts: a pass with exit 1, a fail with exit 0
cat > "$tmp/r-bad.json" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 1, "receipt": "u.r.claim.2026-11-24.gate.tuna.abcdefabcdef.pass"}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
if receipts "$tmp/r-bad.json" >/dev/null 2>&1; then fail "pass receipt with exit 1 accepted"; fi

cat > "$tmp/r-bad2.json" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 0, "receipt": "u.r.claim.2026-11-24.gate.tuna.abcdefabcdef.fail"}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
if receipts "$tmp/r-bad2.json" >/dev/null 2>&1; then fail "fail receipt with exit 0, claimed pass accepted"; fi

# a MISMATCH is never "unchecked but fine"
cat > "$tmp/r-mismatch.json" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 0, "receipt": "u.r.claim.2026-11-24.gate.tuna.abcdefabcdef.mismatch"}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
if receipts "$tmp/r-mismatch.json" >/dev/null 2>&1; then fail "MISMATCH receipt accepted"; fi

# an unresolvable receipt fails rather than being skipped
cat > "$tmp/r-unresolvable.json" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 0, "receipt": "u.r.claim.2026-11-24.gate.tuna.abcdefabcdef.unresolvable"}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
if receipts "$tmp/r-unresolvable.json" >/dev/null 2>&1; then fail "unresolvable receipt accepted"; fi

# a reproduced unverified verdict cannot support a claimed pass
cat > "$tmp/r-unverified.json" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 0, "receipt": "u.r.claim.2026-11-24.gate.tuna.abcdefabcdef.unverified"}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
if receipts "$tmp/r-unverified.json" >/dev/null 2>&1; then fail "unverified receipt with claimed pass accepted"; fi

# a claim that verifies but carries no verdict line is not a gate receipt
cat > "$tmp/r-noverdict.json" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 0, "receipt": "u.r.claim.2026-11-24.observed.deploy.abcdefabcdef"}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
if receipts "$tmp/r-noverdict.json" >/dev/null 2>&1; then fail "receipt with no verdict line accepted"; fi

# a command without a receipt is not a receipt failure
cat > "$tmp/r-none.json" <<'JSON'
{
  "commands": [{"command": "dune runtest nomia", "exit": 0}],
  "criteria": [{"id": "A1", "evidence": "claim"}]
}
JSON
receipts "$tmp/r-none.json" >/dev/null || fail "receiptless command failed receipt mode"

# without --receipts the receipts are ignored: no verify, no failure
if "$script" "$tmp/r-bad.json" >/dev/null 2>&1; then :; else fail "completeness check changed without --receipts"; fi

echo "review-change script fixtures: PASS"
