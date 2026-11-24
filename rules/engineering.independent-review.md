---
id: engineering.independent-review
revision: 1
owner: wyo-engineering
strength: must
scope: [code-change]
final: true
kind: instruction
title: Keep delivery review genuinely independent
enforcement:
  class: gate
  controls: [ci-review]
actions: [delivery.review]
key: review.independence
value: required
---
Where the delivery process requires review, the reviewer must be independent
of the author and must judge each acceptance criterion with specific evidence.
A reviewer does not approve work it authored, and an implementer does not
approve its own change by relabeling the pass.

Review findings name a file/line or command. A missing check is recorded as
unrun, not converted into approval.
