---
rules:
  - id: repo.locked
    revision: 1
    owner: wyo-engineering
    strength: should
    scope: [code-change]
    final: false
    kind: instruction
    key: repo.locked
    value: enabled
---
A fragment selected by the lock rather than by AGENTS.md.
