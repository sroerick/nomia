---
id: org.consent
revision: 1
owner: wyo-engineering
strength: must
scope: [all]
final: true
kind: instruction
title: Consent fixture rule
enforcement:
  class: consent
  controls: [host-hook]
actions: [vcs.push]
key: consent.key
value: required
---
A consent-gated action that needs a host hook.
