---
id: knowledge.attributed-store
revision: 1
owner: wyo-platform
strength: must
scope: [all]
final: true
kind: instruction
title: Preserve durable knowledge only in its granted store
enforcement:
  class: gate
  controls: [evidence-verifier]
actions: [memory.put]
key: memory.promotion
value: granted-store-only
---
Preserve durable knowledge only through its granted, attributed store. A memory
write names the role, lesson, evidence, and supersession it relies on. Do not
turn task progress, logs, or a single observation into organization policy or
unattributed global memory.

A claim's content hash establishes identity, not truth. Formal verification and
ordinary model-authored memory are different authorities.
