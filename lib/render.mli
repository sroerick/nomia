(** Deterministic rendering of one resolved PolicySet.

    Rendering is a pure function of the composed policy set. It reads skill
    bytes from the source root only to copy them into the selected client's
    skill directory; every byte is checked against the hash recorded by
    {!Model.skill.files}. No wall-clock value, absolute machine path, or
    filesystem enumeration order participates in an artifact. *)

type artifact = {
  path : string;  (** Relative to the render output directory. *)
  contents : string;
  mode : int;  (** [0o755] for scripts, [0o644] otherwise. *)
}

val check_out : out:string -> (unit, string) result
(** [check_out ~out] succeeds when [out] is absent or is an empty directory.
    A regular file, an unreadable path, or a non-empty directory is an error
    with an actionable message. It never creates or modifies [out]. *)

val write : out:string -> artifact list -> (unit, string) result
(** [write ~out artifacts] refuses a non-empty or non-directory [out] before
    writing anything, creates missing parent directories, and writes each
    artifact with its exact bytes and mode. Artifact paths must be non-empty
    and relative with no [.] or [..] component, so no write escapes [out]. *)

val render :
  Compose.t -> client:string -> (artifact list, Model.diagnostic list) result
(** [render policy_set ~client] renders [client]. Supported clients in this
    card are [generic-agents], [opencode], and [erde]. [generic-agents] and
    [opencode] artifacts always include [AGENTS.md], [.nomia/explain.json],
    and every file of every skill under the adapter's skill directory.
    [opencode] additionally emits [opencode.json]. [erde] instead emits
    [org-core.md], [framework.md], [policy.json], the same explain document,
    and skills under the adapter's [skills] directory. The release manifest is
    built and written by the caller after these artifacts exist; it is not part
    of this list.

    An unknown client, a client that does not match the composed adapter, a
    skill file whose bytes no longer match the loaded source model, or an erde
    memory-contract fragment that is missing, changed, or absent from
    [source.files] is a diagnostic. *)
