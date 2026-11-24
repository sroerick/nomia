#!/usr/bin/env python3
"""check-evidence.sh -- validate a structured review evidence report.

Usage: check-evidence.sh FILE

Expected JSON:
  {
    "commands": [{"command": "...", "exit": 0}],
    "criteria": [{"id": "...", "evidence": "..."}],
    "unrun": ["..."]
  }

This is a completeness check, not a truth check. It is offline and read-only.
It fails on a missing file, malformed JSON, a command without an integer exit,
an empty criterion/evidence, or a claimed pass with a nonzero exit.
"""
import json
import sys


def fail(msg):
    print("check-evidence: %s" % msg, file=sys.stderr)
    return 1


def main(argv):
    if len(argv) != 2:
        return fail("usage: check-evidence.sh FILE")
    try:
        with open(argv[1], "r", encoding="utf-8") as fh:
            data = json.load(fh)
    except OSError as exc:
        return fail("cannot read %s: %s" % (argv[1], exc))
    except json.JSONDecodeError as exc:
        return fail("invalid JSON: %s" % exc)
    if not isinstance(data, dict):
        return fail("top level must be an object")

    commands = data.get("commands")
    criteria = data.get("criteria")
    unrun = data.get("unrun", [])
    if not isinstance(commands, list) or not commands:
        return fail("commands must be a non-empty list")
    if not isinstance(criteria, list) or not criteria:
        return fail("criteria must be a non-empty list")
    if not isinstance(unrun, list):
        return fail("unrun must be a list")

    for i, cmd in enumerate(commands):
        if not isinstance(cmd, dict):
            return fail("command %d is not an object" % i)
        if not isinstance(cmd.get("command"), str) or not cmd["command"].strip():
            return fail("command %d has no command text" % i)
        code = cmd.get("exit")
        if not isinstance(code, int):
            return fail("command %d has no integer exit" % i)
        if code != 0 and cmd.get("claimed_pass") is True:
            return fail("command %d claims a pass with exit %d" % (i, code))

    seen = set()
    for i, crit in enumerate(criteria):
        if not isinstance(crit, dict):
            return fail("criterion %d is not an object" % i)
        cid = crit.get("id")
        if not isinstance(cid, str) or not cid.strip():
            return fail("criterion %d has no id" % i)
        if cid in seen:
            return fail("duplicate criterion id: %s" % cid)
        seen.add(cid)
        ev = crit.get("evidence")
        if not isinstance(ev, str) or not ev.strip():
            return fail("criterion %s has no evidence" % cid)

    print("check-evidence: OK (%d commands, %d criteria, %d unrun)"
          % (len(commands), len(criteria), len(unrun)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
