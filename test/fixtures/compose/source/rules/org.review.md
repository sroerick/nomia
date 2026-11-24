---
id: org.review
revision: 1
owner: wyo-engineering
strength: must
scope: [code-change]
final: true
kind: instruction
title: Review fixture rule
enforcement:
  class: gate
  controls: [ci-review]
actions: [delivery.review]
key: review.key
value: required
---
A final rule whose review control most adapters cannot provide.
