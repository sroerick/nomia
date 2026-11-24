---
rules:
  - id: repo.fragment-one
    revision: 1
    owner: wyo-engineering
    strength: should
    scope: [code-change]
    final: false
    kind: instruction
relaxes: [engineering.scope]
---
Fragment that relaxes an organization rule.
