#!/usr/bin/env python3
"""check-evidence.sh -- validate a structured review evidence report.

Usage: check-evidence.sh [--receipts] FILE

Expected JSON:
  {
    "commands": [{"command": "...", "exit": 0,
                  "receipt": "u.<runner>.claim.<date>.gate.<project>.<rev12>.<gate>"}],
    "criteria": [{"id": "...", "evidence": "..."}],
    "unrun": ["..."]
  }

This is a completeness check, not a truth check. It is offline and read-only.
It fails on a missing file, malformed JSON, a command without an integer exit,
an empty criterion/evidence, or a claimed pass with a nonzero exit.

--receipts (borg/gate-receipts.borg, G2) additionally resolves each command's
optional `receipt` -- the claim path a gate receipt minted for it -- with
`claims.sh verify`, and fails when the receipt's verdict (the gate verdict
trees: pass 10, fail 110, unverified 1110) disagrees with the command's
claimed exit or pass.  A MISMATCH, an unresolvable receipt, and a reproduced
unverified verdict against a claimed pass all fail the check: never "unchecked
but fine".  A command without a `receipt` is not a receipt failure, and
without the mode the receipts are ignored and nothing is executed.

Env: CLAIMS_SH (default: the repository's tuna/scripts/claims.sh).
"""
import json
import os
import subprocess
import sys

VERDICTS = ("pass", "fail", "unverified")


def fail(msg):
    print("check-evidence: %s" % msg, file=sys.stderr)
    return 1


def claims_sh():
    override = os.environ.get("CLAIMS_SH")
    if override:
        return override
    root = os.path.dirname(os.path.abspath(__file__))
    for _ in range(6):
        candidate = os.path.join(root, "tuna", "scripts", "claims.sh")
        if os.path.isfile(candidate):
            return candidate
        root = os.path.dirname(root)
    return os.path.join(os.getcwd(), "tuna", "scripts", "claims.sh")


def verify_receipt(i, cmd, claims):
    """Resolve one receipt; return None when it agrees, else a message."""
    path = cmd.get("receipt")
    if path is None:
        return None
    if not isinstance(path, str) or not path.strip():
        return "command %d has a receipt that is not a claim path" % i
    path = path.strip()
    code = cmd.get("exit")
    claimed_pass = code == 0 or cmd.get("claimed_pass") is True
    try:
        proc = subprocess.run(["bash", claims, "verify", path],
                              capture_output=True, text=True)
    except OSError as exc:
        return "command %d receipt %s is unresolvable: %s" % (i, path, exc)
    out = (proc.stdout or "") + (proc.stderr or "")
    verdict = None
    for line in out.splitlines():
        if line.startswith("verdict="):
            verdict = line.split("=", 1)[1].strip()
    if "MISMATCH" in out:
        return "command %d receipt %s does not reproduce (MISMATCH)" % (i, path)
    if proc.returncode != 0:
        return ("command %d receipt %s is unresolvable (claims.sh verify exited %d)"
                % (i, path, proc.returncode))
    if verdict is None:
        return ("command %d receipt %s carries no verdict (not a gate receipt)"
                % (i, path))
    if verdict not in VERDICTS:
        return "command %d receipt %s names an unknown verdict '%s'" % (i, path, verdict)
    if verdict == "pass" and not claimed_pass:
        return ("command %d receipt %s says pass but the command claims exit %d"
                % (i, path, code))
    if verdict == "fail" and claimed_pass:
        return ("command %d receipt %s says fail but the command claims a pass"
                % (i, path))
    if verdict == "unverified" and claimed_pass:
        return ("command %d receipt %s is unverified, so it cannot support a claimed pass"
                % (i, path))
    print("check-evidence: command %d receipt=%s verdict=%s" % (i, path, verdict))
    return None


def main(argv):
    args = argv[1:]
    receipts = False
    files = []
    for arg in args:
        if arg == "--receipts":
            receipts = True
        else:
            files.append(arg)
    if len(files) != 1:
        return fail("usage: check-evidence.sh [--receipts] FILE")
    try:
        with open(files[0], "r", encoding="utf-8") as fh:
            data = json.load(fh)
    except OSError as exc:
        return fail("cannot read %s: %s" % (files[0], exc))
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

    if receipts:
        claims = claims_sh()
        for i, cmd in enumerate(commands):
            msg = verify_receipt(i, cmd, claims)
            if msg is not None:
                return fail(msg)

    print("check-evidence: OK (%d commands, %d criteria, %d unrun)"
          % (len(commands), len(criteria), len(unrun)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
