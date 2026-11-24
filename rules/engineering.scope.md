---
id: engineering.scope
revision: 1
owner: wyo-engineering
strength: must
scope: [code-change]
final: true
kind: instruction
title: Stay inside the requested scope
enforcement:
  class: advisory
  controls: [instruction]
actions: [repo.write]
key: change.scope
value: bounded
---
Keep the change inside the requested scope. Do not edit unrelated files,
reformat adjacent code, or alter another person's uncommitted work. If the task
cannot be completed without expanding scope, stop and ask rather than absorbing
the extra work silently.

Follow the project's existing structure, naming, and style. A smaller change
that fits the codebase is better than a broader rewrite that erases local
conventions.
