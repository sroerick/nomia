---
id: engineering.smallest-change
revision: 1
owner: wyo-engineering
strength: should
scope: [code-change]
final: false
kind: instruction
title: Prefer the smallest maintainable change
enforcement:
  class: advisory
  controls: [instruction]
actions: [repo.write]
key: change.style
value: smallest-maintainable
---
Make the smallest maintainable change in the existing project style. Prefer
reusing an existing seam to inventing a parallel abstraction. Delete
speculative generality and avoid a new dependency when the standard library or
an already-installed package covers the need.

This rule is advisory. It guides judgment; it is not a mechanical limit on the
size of a justified change.
