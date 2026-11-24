---
id: org.gate
revision: 1
owner: wyo-engineering
strength: must
scope: [code-change]
final: true
kind: instruction
title: Gate fixture rule
enforcement:
  class: gate
  controls: [ci-verification]
actions: [delivery.verify]
key: gate.key
value: required
---
A gated final rule used to prove capability mapping.
