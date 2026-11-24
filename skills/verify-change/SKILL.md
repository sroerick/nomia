---
name: verify-change
description: >
  Select and run focused verification for a change, then report exact commands
  and results. Use before claiming a change is verified or requesting review.
triggers:
  - before marking a code change verified
  - before requesting independent review
  - when deciding which checks a diff requires
prerequisites:
  - a working tree containing the change
  - the project's own documented check commands
stop_conditions:
  - a required tool or fixture is unavailable
  - the repository instructions name a different mandatory gate
  - a check would touch a live service or credential without a sandbox
verification:
  - every reported command has an exit status
  - unavailable checks are named as unrun
  - a red check blocks the verified claim
rules:
  - engineering.verify-change
---
# Verify a change

1. Read the repository instructions and the card's exact acceptance criteria.
   Those gates outrank any generic suggestion below.
2. List the changed paths and identify the smallest set of checks that proves
   the behavior, then the broader gate the project requires for delivery.
3. Use `scripts/select-checks.sh REPO PATH...` as a starting point when the
   repository has no explicit command list. Treat its output as suggestions,
   not evidence.
4. Run the focused checks first, then the broader gate. Capture the exact
   command and exit status.
5. Report:
   - commands run, with exit status;
   - what each command proves;
   - checks that could not run and why;
   - any pre-existing failure, classified separately from the change.
6. Do not call a check a pass because it was unavailable, skipped, or only
   described. Do not mark the change verified while a required check is red.

A harness failure is not a code verdict. Record it as `UNRUNNABLE`, keep the
work unverified, and name the harness blocker.
