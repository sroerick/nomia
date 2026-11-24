# Nomia

Organization-wide agent instructions, procedures, role composition, and native
client adapters. `agentctl` compiles the reviewed source into immutable,
content-addressed releases and installs them atomically. It is an operator and
maintainer tool: it is not an agent tool, a model gateway, a session
synchronizer, or a resident service.

**Status:** implemented. The Borge book is 100% implemented and the offline
acceptance gate is green:

```sh
nomia/scripts/verify_nomia.sh
```

The authoritative design is [`nomia.borg`](nomia.borg). The
implementation plan, card graph, and evidence log are in
[`../docs/handoffs/2026-11-23-agent-policy-ralph-loop.md`](../docs/handoffs/2026-11-23-agent-policy-ralph-loop.md);
the final report is
[`../docs/handoffs/2026-11-23-agent-policy-report.md`](../docs/handoffs/2026-11-23-agent-policy-report.md).
The earlier discussion document is
[`../docs/nomia-plane.md`](../docs/nomia-plane.md); where they
differ, the Borge book wins.

## What it does

- One canonical, reviewed source for organization rules, roles, skills, and
  adapter capabilities.
- Deterministic, client-scoped releases. The selected adapter is part of the
  release identity, so a generic release and an OpenCode release never collide
  in the cache or in MCP.
- Monorepo project contexts: one repository root fragment plus exactly one
  explicit project fragment, with ordered provenance, no sibling leakage, and
  one root pin serving every project install.
- Native renderings for generic `AGENTS.md` clients, OpenCode v2, Erde, and
  Saguaro; no lowest-common-denominator prompt.
- `agentctl check` for policy-author validation, `render` for offline
  generation, and `explain` for provenance.
- `sync` stages and verifies a complete release before an atomic `current`
  swap; `doctor` distinguishes current, stale, missing, unmanaged,
  hand-edited, partially enforced, and offline last-known-good targets.
- A trusted `decide` seam for host wrappers, with deny-precedence and honest
  unsupported-control reporting.
- An optional read-only MCP adapter over the same pinned releases.
- No daemon, network dependency, credential, model proxy, or autonomous
  publication path.

## Layout

```text
nomia.borg       authoritative specification
policy.yaml             schema, release metadata, maintainers, core budget
rules/                  normative Markdown rules with typed front matter
skills/                 reviewed procedures and their offline fixtures
roles/                  shared role charters
adapters/               capability matrices and render configuration
exceptions.yaml         reviewed, scoped, expiring exceptions
fragments/              reviewed adapter inputs (Erde memory contract,
                        Saguaro persona template), hashed into releases
schemas/                source, lock, exception, and release schemas
lib/                    pure parsing, validation, composition, release,
                        rendering, enforcement, store, sync, and MCP seams
bin/                    agentctl entry point
scripts/                offline acceptance gate
test/                   unit, fixture, golden, CLI, sync, and MCP suites
```

## Quick start

```sh
eval $(opam env --switch=poohstack --set-switch)

# Validate the reviewed source.
_build/default/nomia/bin/agentctl.exe check nomia

# Render one client into a fresh directory.
_build/default/nomia/bin/agentctl.exe render \
  --source nomia --client opencode --role implementer --out /tmp/policy

# Inspect the exact PolicySet and every rule's source.
_build/default/nomia/bin/agentctl.exe explain \
  --source nomia --client opencode --role implementer --json

# Compose the wyo root and one project; siblings are excluded.
_build/default/nomia/bin/agentctl.exe explain \
  --source nomia --repo . --project tuna --client opencode --json

# Pin the monorepo root and install one project-scoped target.
_build/default/nomia/bin/agentctl.exe sync \
  --repo . --project tuna --client opencode --source nomia --update-pin
_build/default/nomia/bin/agentctl.exe doctor \
  --repo . --project tuna --source nomia

# User-level managed install (under ~/.config/nomia/clients/<client>).
_build/default/nomia/bin/agentctl.exe sync --client opencode --source nomia
```

`render` writes only into an empty `--out` directory. `sync` is the atomic
installation path. `decide` reads one normalized host action from stdin and
exits `0` allow, `3` deny, `4` ask, or `1` for a usage/policy error.

```sh
printf '%s' '{"action":"vcs.push","tool":"shell"}' |
  agentctl decide --source nomia --client opencode --json
```

The optional read-only MCP server speaks JSON-RPC over stdio:

```sh
agentctl mcp --source nomia --client opencode
```

## Design notes and deliberate boundaries

- **Strict YAML subset.** No YAML library is installed, so `lib/yaml_lite.ml`
  implements a real parser for the narrow schema. Tabs, anchors, aliases,
  tags, directives, duplicate keys, multi-document input, and unsupported
  constructs are hard errors with line numbers. It is not a regex front end.
- **Client-scoped release digest.** The selected adapter is part of the input
  digest and manifest. `--release DIGEST` names exactly one client's release;
  asking MCP for a different client is a clean error.
- **Repository pins are explicit and monorepo-wide.** `sync --repo` requires a
  `nomia.lock`; `--update-pin` is the reviewed re-pin operation. The pin
  records the organization release and source-tree digest, so one root pin
  serves every `--project` context. A project install targets
  `<repo>/<project>` and does not create a second lock. A directory without a
  pin does not silently inherit a source release. The explicitly selected user
  default is the `sync --client` target.
- **Projects are explicit; siblings never leak.** `--project PATH` is relative
  to `--repo` and adds that project's reviewed fragment after the root
  fragment. The compiler never infers a project from changed files or the
  current directory. `project:` exception scopes and request-project metadata
  use the same identity.
- **Path-only exceptions are rejected.** Render has no changed-file context,
  so it cannot honestly prove a `path:` scope. Use a repository or project
  scope, which may still carry a path qualifier.
- **Render is a fresh-directory operation; sync is atomic.** Adapters declare
  `atomic_output` because the managed install path stages, verifies, and swaps
  a complete release. `render` itself refuses a non-empty target and never
  edits native configuration.
- **Instructions are not enforcement.** Unsupported controls are reported as
  advisory/unsupported in explain and doctor. OpenCode's native permission
  `ask` is counted as a partial consent control; no pre-tool hook is invented.
  Erde keeps its typed grant boundary.
- **Offline first.** Check, render, explain, sync, doctor, decide, and MCP do
  not require a provider, network, credential, Tuna, or Pricklypear. Cached and
  installed releases support last-known-good operation; no outage produces an
  empty policy.

## Development

The component is part of the root dune project on the `poohstack` switch.

```sh
eval $(opam env --switch=poohstack --set-switch)
dune build nomia
dune runtest nomia --force
dune build --root erde && dune runtest --root erde --force
python3 saguaro/harness/scripts/test_gen_agents_policy.py
```

The full offline gate also runs the skill fixtures, Borge balance/lint/check
and a 100% completion check, the five malformed-corpus diagnostics, two
byte-identical renders, explain/render equality, enforcement decisions, atomic
sync with an injected pre-swap fault, unmanaged and hand-edited doctor states,
user-level sync, MCP lifecycle, and path/secret hygiene:

```sh
nomia/scripts/verify_nomia.sh
```
