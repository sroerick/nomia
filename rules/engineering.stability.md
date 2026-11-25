---
id: engineering.stability
revision: 1
owner: wyo-engineering
strength: should
scope: [code-change]
final: false
kind: instruction
title: Match the change to the project's stability
enforcement:
  class: advisory
  controls: [instruction]
actions: [repo.write]
key: change.stability
value: declared-label
---
Versions follow SemVer: MAJOR counts breaking changes, MINOR adds features,
PATCH fixes bugs, and `0.x` promises nothing. Maturity is a separate stability
label, declared in the project's book or `AGENTS.fragment.md`:

- `experimental` — dogfooded and unstable. Propose improvements freely,
  including breaking ones.
- `beta` — shape mostly settled. Propose improvements; name any break and
  what it costs callers.
- `stable` — do not break the API or userland. A break needs a major bump and
  an owner decision; propose it, do not make it.
- `lts` — stable with support commitments. Default to fixes only.
- `deprecated` — point to the replacement; do not extend.

With no label, treat a `0.x` version as `beta` and anything else as `stable`,
and say the label is missing.
