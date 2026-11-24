---
name: broken
description: Skill with a dangling rule reference.
triggers:
  - the broken fixture runs
prerequisites:
  - a fixture root
stop_conditions:
  - the reference is missing
verification:
  - the fixture shows the dangling reference
rules:
  - test.missing
---
# Broken reference

This skill points at a rule that does not exist.
