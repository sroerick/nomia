---
name: review-change
description: >
  Review a change independently against its acceptance criteria, with evidence
  per criterion and an honest list of unrun checks. Use for delivery review.
triggers:
  - a change is ready for independent review
  - a reviewer must judge acceptance criteria
  - checking whether a verification report is complete
prerequisites:
  - the change diff and its acceptance criteria
  - the author's reported commands and evidence
stop_conditions:
  - the reviewer authored the change
  - criteria are missing or untestable
  - review requires live credentials or production state
verification:
  - every criterion has file/line or command evidence
  - every claimed check has an exit status
  - the verdict is APPROVE or REQUEST-CHANGES
rules:
  - engineering.independent-review
  - engineering.verify-change
---
# Review a change

1. Confirm independence. If you wrote the change, stop; another reviewer must
   take the pass.
2. Read the acceptance criteria before reading the diff, then check each
   criterion with a specific file/line or command result.
3. Look for scope creep, silent behavior changes, tests that pass for the wrong
   reason, weakened assertions, secrets, and enforcement claims the adapter
   cannot support.
4. Re-run read-only checks when practical. Do not edit the author's work.
5. Validate the author's evidence with
   `scripts/check-evidence.sh EVIDENCE.json` when the report is structured.
   A command may carry a `receipt` (the claim path a gate receipt minted);
   `--receipts` resolves each with `claims.sh verify` and fails on a
   MISMATCH, an unresolvable receipt, or a verdict that disagrees with the
   claimed exit or pass.
6. Write a verdict per criterion and one overall verdict:
   `APPROVE` or `REQUEST-CHANGES`. Name residual risks and unrun checks.
   A harness failure is not a code failure; record it as unrun.

Review is not a courtesy approval. A missing or unverifiable criterion is a
request for changes, not a pass.
