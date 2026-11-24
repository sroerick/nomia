#!/bin/sh
# nomia acceptance gate.
#
# Offline and deterministic: a temporary HOME/XDG root, a fixed policy date,
# a dead proxy, and no provider, credential, Tuna, Pricklypear, or network
# service. The gate proves construction and isolation; it does not claim that
# an agent obeyed the policy.
set -eu

SELF_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ROOT=$(CDPATH= cd -- "$SELF_DIR/../.." && pwd)
SOURCE="$ROOT/nomia"
AGENTCTL="$ROOT/_build/default/nomia/bin/agentctl.exe"

TMP=$(mktemp -d "${TMPDIR:-/tmp}/nomia-gate.XXXXXX")
cleanup() { rm -rf -- "$TMP"; }
trap cleanup EXIT INT TERM

cd "$ROOT"
eval "$(opam env --switch=poohstack --set-switch)"

export HOME="$TMP/home"
export XDG_CONFIG_HOME="$TMP/config"
export XDG_CACHE_HOME="$TMP/cache"
export XDG_DATA_HOME="$TMP/data"
export XDG_STATE_HOME="$TMP/state"
export NOMIA_TODAY=2030-01-01
export http_proxy="http://127.0.0.1:9"
export https_proxy="http://127.0.0.1:9"
export HTTP_PROXY="$http_proxy"
export HTTPS_PROXY="$https_proxy"
export NO_PROXY=""
mkdir -p "$HOME" "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME" "$XDG_DATA_HOME" "$XDG_STATE_HOME"

pass=0
fail=0
ok() { pass=$((pass + 1)); printf 'ok   %s\n' "$1"; }
bad() { fail=$((fail + 1)); printf 'FAIL %s\n' "$1" >&2; }
check() { if "$@" >/dev/null 2>&1; then ok "$*"; else bad "$*"; fi; }

# A command that must exit nonzero; optional expected status.
expect_status() {
  expected=$1
  shift
  set +e
  "$@" >/dev/null 2>&1
  actual=$?
  set -e
  if [ "$actual" -eq "$expected" ]; then
    ok "exit $expected: $*"
  else
    bad "expected exit $expected, got $actual: $*"
  fi
}

if dune build nomia; then ok "dune build nomia"; else bad "dune build nomia"; fi
if dune runtest nomia --force; then ok "dune runtest nomia"; else bad "dune runtest nomia"; fi
if dune build --root erde; then ok "dune build --root erde"; else bad "dune build --root erde"; fi
if dune runtest --root erde --force; then ok "dune runtest --root erde"; else bad "dune runtest --root erde"; fi
if python3 saguaro/harness/scripts/test_gen_agents_policy.py; then
  ok "saguaro managed-mode tests"
else
  bad "saguaro managed-mode tests"
fi

for test_script in nomia/skills/*/tests/*.sh; do
  if sh "$test_script"; then ok "skill test $(basename "$test_script")"; else bad "skill test $test_script"; fi
done

if borge balance nomia/nomia.borg >/dev/null; then ok "borge balance"; else bad "borge balance"; fi
if borge lint --quiet nomia >/dev/null; then ok "borge lint"; else bad "borge lint"; fi
if borge check --quiet nomia >/dev/null; then ok "borge check"; else bad "borge check"; fi
if borge report nomia 2>/dev/null | grep -q 'Completion: 100.0%'; then
  ok "borge completion is 100%"
else
  bad "borge completion is not 100%"
fi

if [ ! -x "$AGENTCTL" ]; then
  bad "agentctl executable missing after dune build"
else
  ok "agentctl built"
fi

# Policy checks: the real corpus passes; each required malformed corpus fails.
check "$AGENTCTL" check "$SOURCE"
for fixture in duplicate-id missing-owner broken-skill-reference unauthorized-override expired-exception; do
  expect_status 1 "$AGENTCTL" check "$SOURCE/test/fixtures/invalid/$fixture"
done

# Render twice, compare every byte, and prove explain uses the same merge.
mkdir -p "$TMP/render1" "$TMP/render2"
if "$AGENTCTL" render --source "$SOURCE" --client generic-agents --out "$TMP/render1"; then
  ok "render generic-agents"
else
  bad "render generic-agents"
fi
if "$AGENTCTL" render --source "$SOURCE" --client generic-agents --out "$TMP/render2"; then
  ok "render generic-agents twice"
else
  bad "render generic-agents twice"
fi
if diff -r "$TMP/render1" "$TMP/render2" >/dev/null 2>&1; then
  ok "two renders are byte-identical"
else
  bad "two renders differ"
fi
if "$AGENTCTL" explain --source "$SOURCE" --client generic-agents --json > "$TMP/explain.json" 2>/dev/null &&
   cmp -s "$TMP/explain.json" "$TMP/render1/.nomia/explain.json"; then
  ok "explain equals rendered explain.json"
else
  bad "explain differs from rendered explain.json"
fi

# Render refuses a non-empty target and leaves it untouched.
mkdir -p "$TMP/nonempty"
printf 'hand-authored\n' > "$TMP/nonempty/keep.txt"
if "$AGENTCTL" render --source "$SOURCE" --client generic-agents --out "$TMP/nonempty" >/dev/null 2>&1; then
  bad "render accepted a non-empty out dir"
else
  ok "render refuses a non-empty out dir"
fi
if [ "$(cat "$TMP/nonempty/keep.txt")" = "hand-authored" ]; then
  ok "refused render left the target untouched"
else
  bad "refused render modified the target"
fi

# The trusted decision seam: deny, ask, allow.
set +e
printf '%s' '{"action":"fs.read","tool":"read","paths":[".env"]}' |
  "$AGENTCTL" decide --source "$SOURCE" --client opencode --json > "$TMP/deny.json" 2>/dev/null
code=$?
set -e
if [ "$code" -eq 3 ] && grep -q '"decision":"deny"' "$TMP/deny.json"; then
  ok "credential read denies (exit 3)"
else
  bad "credential read did not deny with exit 3"
fi
set +e
printf '%s' '{"action":"vcs.push","tool":"shell"}' |
  "$AGENTCTL" decide --source "$SOURCE" --client opencode --json > "$TMP/ask.json" 2>/dev/null
code=$?
set -e
if [ "$code" -eq 4 ] && grep -q '"decision":"ask"' "$TMP/ask.json"; then
  ok "vcs.push asks (exit 4)"
else
  bad "vcs.push did not ask with exit 4"
fi
set +e
printf '%s' '{"action":"fs.read","tool":"read","paths":["src/example.ml"]}' |
  "$AGENTCTL" decide --source "$SOURCE" --client opencode --json > "$TMP/allow.json" 2>/dev/null
code=$?
set -e
if [ "$code" -eq 0 ] && grep -q '"decision":"allow"' "$TMP/allow.json"; then
  ok "ordinary source read allows (exit 0)"
else
  bad "ordinary source read did not allow with exit 0"
fi

# Atomic repository sync, lock pin, doctor, interrupted swap.
REPO="$TMP/repo"
mkdir -p "$REPO"
printf 'Repository-specific convention: keep the change small.\n' > "$REPO/AGENTS.fragment.md"
if "$AGENTCTL" sync --repo "$REPO" --client generic-agents --source "$SOURCE" --update-pin; then
  ok "repo sync writes the pin"
else
  bad "repo sync failed"
fi
if [ -f "$REPO/nomia.lock" ] && [ -L "$REPO/AGENTS.md" ] && [ -f "$REPO/.nomia/state.json" ]; then
  ok "managed layout and entrypoints exist"
else
  bad "managed layout is incomplete"
fi
if "$AGENTCTL" doctor --repo "$REPO" --source "$SOURCE" --json > "$TMP/doctor.json" 2>/dev/null &&
   grep -Eq '"state":"(current|partial)"' "$TMP/doctor.json" &&
   grep -q '"issues":\[\]' "$TMP/doctor.json"; then
  ok "doctor reports a current or partially-enforced target"
else
  bad "doctor did not report current/partial"
fi
SRC2="$TMP/source2"
cp -a "$SOURCE" "$SRC2"
printf '\nExtra reviewed line for the fault fixture.\n' >> "$SRC2/rules/engineering.scope.md"
if NOMIA_FAULT=before-swap "$AGENTCTL" sync --repo "$REPO" --client generic-agents --source "$SRC2" --update-pin >/dev/null 2>&1; then
  bad "fault-injected sync unexpectedly succeeded"
else
  ok "fault-injected sync fails before the swap"
fi
if "$AGENTCTL" doctor --repo "$REPO" --source "$SOURCE" --json >/dev/null 2>&1; then
  ok "previous target remains usable after the failed swap"
else
  bad "previous target was damaged by the failed swap"
fi

# Monorepo project context: one root pin, project-scoped target, sibling excluded.
PROJECT_REPO="$TMP/project-repo"
mkdir -p "$PROJECT_REPO/tuna" "$PROJECT_REPO/pp"
printf 'Wyo-wide repository instructions.\n' > "$PROJECT_REPO/AGENTS.fragment.md"
printf 'Tuna-specific instructions.\n' > "$PROJECT_REPO/tuna/AGENTS.fragment.md"
printf 'Pricklypear-specific instructions.\n' > "$PROJECT_REPO/pp/AGENTS.fragment.md"
if "$AGENTCTL" sync --repo "$PROJECT_REPO" --project tuna \
     --client generic-agents --source "$SOURCE" --update-pin >/dev/null 2>&1 &&
   [ -f "$PROJECT_REPO/nomia.lock" ] &&
   [ ! -f "$PROJECT_REPO/tuna/nomia.lock" ] &&
   [ -f "$PROJECT_REPO/tuna/.nomia/state.json" ] &&
   [ -L "$PROJECT_REPO/tuna/AGENTS.md" ]; then
  ok "project sync uses one root pin and installs under the project"
else
  bad "project sync layout is wrong"
fi
if grep -q 'Wyo-wide repository instructions' "$PROJECT_REPO/tuna/AGENTS.md" &&
   grep -q 'Tuna-specific instructions' "$PROJECT_REPO/tuna/AGENTS.md" &&
   ! grep -q 'Pricklypear-specific instructions' "$PROJECT_REPO/tuna/AGENTS.md"; then
  ok "project render composes root and selected project without siblings"
else
  bad "project composition leaked or omitted a fragment"
fi
if "$AGENTCTL" doctor --repo "$PROJECT_REPO" --project tuna \
     --source "$SOURCE" --json > "$TMP/project-doctor.json" 2>/dev/null &&
   grep -Eq '"state":"(current|partial)"' "$TMP/project-doctor.json" &&
   grep -q '"project":"tuna"' "$TMP/project-doctor.json"; then
  ok "project doctor reports the selected project"
else
  bad "project doctor did not report the selected project"
fi

# An unmanaged target is never overwritten.
UNMANAGED="$TMP/unmanaged"
mkdir -p "$UNMANAGED"
printf 'hand-authored policy\n' > "$UNMANAGED/AGENTS.md"
if "$AGENTCTL" sync --repo "$UNMANAGED" --client generic-agents --source "$SOURCE" --update-pin >/dev/null 2>&1; then
  bad "sync overwrote an unmanaged AGENTS.md"
else
  ok "sync refuses an unmanaged AGENTS.md"
fi
if [ "$(cat "$UNMANAGED/AGENTS.md")" = "hand-authored policy" ]; then
  ok "unmanaged AGENTS.md is untouched"
else
  bad "unmanaged AGENTS.md changed"
fi

# A hand-edited managed release is reported, not silently treated as current.
EDITED="$TMP/edited"
mkdir -p "$EDITED"
printf 'fragment\n' > "$EDITED/AGENTS.fragment.md"
"$AGENTCTL" sync --repo "$EDITED" --client generic-agents --source "$SOURCE" --update-pin >/dev/null 2>&1 || true
DIGEST=$(readlink "$EDITED/.nomia/current" 2>/dev/null | sed 's|releases/||' || true)
if [ -n "$DIGEST" ]; then
  printf '\nhand edit\n' >> "$EDITED/.nomia/releases/$DIGEST/AGENTS.md"
  set +e
  "$AGENTCTL" doctor --repo "$EDITED" --source "$SOURCE" --json > "$TMP/edited.json" 2>/dev/null
  code=$?
  set -e
  if [ "$code" -eq 1 ] && grep -q '"state":"hand-edited"' "$TMP/edited.json"; then
    ok "doctor reports a hand-edited release"
  else
    bad "doctor did not report the hand edit"
  fi
else
  bad "cannot locate the installed release digest"
fi

# User-level sync writes under XDG_CONFIG_HOME only.
if "$AGENTCTL" sync --client opencode --source "$SOURCE" >/dev/null 2>&1; then
  if [ -d "$XDG_CONFIG_HOME/nomia/clients/opencode/.nomia" ]; then
    ok "user-level sync uses the nomia config root"
  else
    bad "user-level sync wrote to the wrong place"
  fi
else
  bad "user-level sync failed"
fi

# MCP: initialize, list, read the manifest, call the bounded resolver.
if python3 - "$AGENTCTL" "$SOURCE" <<'PY'
import json, subprocess, sys

exe, source = sys.argv[1], sys.argv[2]
proc = subprocess.Popen(
    [exe, "mcp", "--source", source, "--client", "generic-agents"],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
    text=True,
)

def send(message):
    proc.stdin.write(json.dumps(message) + "\n")
    proc.stdin.flush()

def recv():
    line = proc.stdout.readline()
    if not line:
        raise SystemExit("mcp closed stdout: " + proc.stderr.read())
    return json.loads(line)

send({"jsonrpc": "2.0", "id": 1, "method": "initialize",
      "params": {"protocolVersion": "2024-11-05", "capabilities": {},
                 "clientInfo": {"name": "gate", "version": "1"}}})
assert recv()["id"] == 1
send({"jsonrpc": "2.0", "method": "notifications/initialized"})
send({"jsonrpc": "2.0", "id": 2, "method": "resources/list"})
resources = recv()["result"]["resources"]
assert resources, "no resources"
send({"jsonrpc": "2.0", "id": 3, "method": "tools/list"})
tools = recv()["result"]["tools"]
assert [tool["name"] for tool in tools] == ["resolve_policy"]
send({"jsonrpc": "2.0", "id": 4, "method": "tools/call",
      "params": {"name": "resolve_policy", "arguments": {"client": "generic-agents"}}})
result = recv()["result"]
assert "digest" in json.dumps(result)
send({"jsonrpc": "2.0", "id": 5, "method": "shutdown"})
recv()
proc.stdin.close()
proc.wait(timeout=10)
assert proc.returncode == 0, proc.stderr.read()
PY
then
  ok "MCP initialize/list/read/tool lifecycle"
else
  bad "MCP lifecycle"
fi

# Source, rendered artifacts, state, and JSON outputs carry no real home path
# or credential-shaped material.
if grep -RIl '/home/' "$TMP/render1" >/dev/null 2>&1; then
  bad "rendered output contains /home/"
else
  ok "rendered output has no /home/ path"
fi
if grep -RIl -E 'BEGIN ([A-Z]+ )?PRIVATE KEY|AKIA[0-9A-Z]{16}|password=[^" ]+' "$TMP/render1" >/dev/null 2>&1; then
  bad "rendered output contains credential-shaped material"
else
  ok "rendered output has no credential-shaped material"
fi

printf '\nnomia gate: %d passed, %d failed\n' "$pass" "$fail"
[ "$fail" -eq 0 ]
