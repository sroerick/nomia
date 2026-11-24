---
rules:
  - id: repo.relax-unknown
    revision: 1
    owner: wyo-engineering
    strength: may
    scope: [all]
    final: false
    kind: instruction
relaxes: [org.missing]
---
Relax a rule that does not exist.
