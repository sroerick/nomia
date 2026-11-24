---
id: org.prohibit
revision: 1
owner: wyo-security
strength: must
scope: [all]
final: true
kind: instruction
title: Prohibition fixture rule
enforcement:
  class: prohibition
  controls: [filesystem, permissions]
actions: [fs.read]
key: secret.key
value: prohibited
---
A prohibition used to prove partial and unsupported adapter mappings.
