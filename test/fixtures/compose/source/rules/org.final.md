---
id: org.final
revision: 1
owner: wyo-engineering
strength: must
scope: [all]
final: true
kind: instruction
title: Final fixture rule
enforcement:
  class: advisory
  controls: [instruction]
actions: [repo.read]
key: final.key
value: required
---
A final organization rule that repository and role layers cannot relax.
