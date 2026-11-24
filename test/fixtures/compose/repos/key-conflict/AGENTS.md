---
rules:
  - id: repo.one
    revision: 1
    owner: wyo-engineering
    strength: should
    scope: [code-change]
    final: false
    kind: instruction
    key: repo.slot
    value: one
  - id: repo.two
    revision: 1
    owner: wyo-engineering
    strength: should
    scope: [code-change]
    final: false
    kind: instruction
    key: repo.slot
    value: two
---
Two repository rules claim the same slot with different values.
