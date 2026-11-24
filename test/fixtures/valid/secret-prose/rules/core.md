---
id: test.core
revision: 1
owner: wyo-engineering
strength: must
scope: [all]
final: false
kind: instruction
title: Fixture core rule
enforcement:
  class: advisory
  controls: [instruction]
actions: [repo.read]
---
Set PASSWORD=$PASSWORD in the environment.
The password variable is documented without a value.
password=
Pass token=$GITHUB_TOKEN from the environment.
Replace password=changeme before use.
The AKIA prefix alone is not a credential.
