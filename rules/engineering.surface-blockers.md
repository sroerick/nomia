---
id: engineering.surface-blockers
revision: 1
owner: wyo-engineering
strength: must
scope: [all]
final: false
kind: instruction
title: Surface blockers instead of improvising
enforcement:
  class: advisory
  controls: [instruction]
actions: [task.report]
key: blockers.reporting
value: required
---
When missing authority, conflicting instructions, or unavailable verification
blocks the task, surface the specific blocker and the exact unblock needed.
Do not invent credentials, bypass a gate, weaken a test, or improvise around a
boundary to make progress look green.

If several decisions are pending, list them once and continue with the parts
that do not need a decision.
