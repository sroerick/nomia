---
id: engineering.worktree-isolation
revision: 1
owner: wyo-engineering
strength: must
scope: [code-change]
final: true
kind: instruction
title: Branch in your own worktree
enforcement:
  class: advisory
  controls: [instruction]
actions: [repo.write]
key: checkout.isolation
value: own-worktree
---
Make branches in a git worktree you created (`git worktree add <dir> -b
<branch>`), never by switching the branch of a checkout someone else may be
working in. People and agents share checkouts; a checkout or switch there moves
their next commit onto your branch without telling them.

Before you commit or push, confirm `git branch --show-current` is the branch you
mean, and push an explicit refspec. Remove your worktree when the branch lands.
