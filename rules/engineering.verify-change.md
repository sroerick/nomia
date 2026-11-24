---
id: engineering.verify-change
revision: 1
owner: wyo-engineering
strength: must
scope: [code-change]
final: true
kind: instruction
title: Verify changes and report evidence honestly
enforcement:
  class: gate
  controls: [ci-verification]
actions: [delivery.verify]
key: verification.required
value: required
---
Run the checks relevant to the files and behavior changed. Report the command,
its exit status, and what it proves. An unrun or unavailable check is
unverified, never a pass. A harness failure is not a code failure, and a code
failure is not excused because the harness also failed.

Delivery requires gate evidence, not a claim that verification probably
happened. Do not mark work complete while a required check is red or unrun.
