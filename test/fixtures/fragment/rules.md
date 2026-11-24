---
rules:
  - id: repo.fragment-one
    revision: 1
    owner: wyo-engineering
    strength: should
    scope: [code-change]
    final: false
    kind: instruction
    actions: [repo.read]
  - id: repo.fragment-two
    revision: 2
    owner: wyo-engineering
    strength: may
    scope: [operations]
    final: false
    kind: procedure
relaxes: []
---
Fragment body used by both inline rules.
