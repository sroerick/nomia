---
id: engineering.read-first
revision: 1
owner: wyo-engineering
strength: must
scope: [code-change, operations]
final: true
kind: instruction
title: Read the applicable instructions first
enforcement:
  class: advisory
  controls: [instruction]
actions: [repo.read]
key: discovery.required
value: required
---
Read the applicable repository instructions and the authoritative specification
before changing behavior. Discover the project's real source of truth instead
of inferring it from a single file or an old comment.

If repository instructions conflict with the organization baseline, surface
the conflict. A lower layer may add or narrow constraints; it may not silently
relax a final organization rule.
