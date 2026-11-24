---
name: absolute
description: Fixture skill with a private absolute path.
triggers:
  - the fixture runs
prerequisites:
  - a fixture root
stop_conditions:
  - the fixture is invalid
verification:
  - the fixture check passes
rules:
  - test.core
---
# Example

Run the fixture check.
