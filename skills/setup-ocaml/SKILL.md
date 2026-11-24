---
name: setup-ocaml
description: >
  Set up and triage the standard OCaml 5 + dune environment used by the wyo
  monorepo. Use when a build fails because the switch, dune, or dependencies
  are wrong.
triggers:
  - a dune or opam command is missing or uses the wrong switch
  - an OCaml build fails on environment or dependency resolution
  - onboarding a worker into the poohstack switch
prerequisites:
  - opam and the poohstack switch exist
  - no network access is required for an already-installed switch
stop_conditions:
  - the required package is not installed and installing it needs the network
  - the task would upgrade or remove the user's switch
verification:
  - check-switch.sh reports the expected switch and versions
  - dune build runs with the selected switch
rules:
  - engineering.verify-change
---
# Set up the OCaml environment

1. Select the switch explicitly; do not rely on an ambient default:
   ```sh
   eval $(opam env --switch=poohstack --set-switch)
   ```
2. Confirm the environment without changing it:
   ```sh
   scripts/check-switch.sh poohstack
   ```
3. Build from the project root. For this component:
   ```sh
   dune build nomia
   dune runtest nomia --force
   ```
4. When a package is missing, report the exact package and the command needed.
   Do not install, upgrade, or remove a switch as an unattended side effect of
   a code task.
5. When a build failure is not environmental, use the focused verification
   procedure in `verify-change` rather than reinstalling the world.
