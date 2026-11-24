---
id: operations.authority
revision: 1
owner: wyo-engineering
strength: must
scope: [all]
final: true
kind: instruction
title: Require authority for high-impact actions
enforcement:
  class: consent
  controls: [host-hook, permissions]
actions: [vcs.push, deploy.publish, shared.mutate, fs.delete, vcs.history]
key: high-impact.authority
value: required
---
Do not publish, deploy, mutate shared state, delete durable data, or rewrite
version-control history without the authority and approval the operation
requires. A consent-gated action needs a trusted host hook, a native permission
prompt, or a wrapper capable of pausing it; asking the model to remember to ask
is not enforcement.

When the host cannot pause the action, the lane is unsupported for unattended
use. Report that gap instead of pretending the prompt prevents it.
