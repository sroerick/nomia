(** A repository pin: [nomia.lock].

    The lock records the reviewed organization release, the source-tree digest,
    the adapter schema and compiler compatibility, and the optional selected
    role, client, and repository-root fragment override. The source digest can
    serve every project context in one monorepo; an optional legacy exact
    [digest] additionally pins one resolved selection. Parsing is strict:
    unknown fields and wrong value types are diagnostics with source lines.

    A lock carries no absolute machine path. [find_in_repo] records the
    conventional relative name [nomia.lock] for diagnostics. *)

type t = {
  schema : string;
  release : string;
  digest : string option;
  source_digest : string option;
  adapter_schema : int;
  compiler : string;
  role : string option;
  client : string option;
  fragment : string option;
  source : string;  (** Relative lock path used in diagnostics. *)
}

val schema : string
(** ["nomia-lock/v2"]. *)

val lock_file : string
(** The conventional repository-relative name ["nomia.lock"]. *)

val parse : path:string -> (t, Model.diagnostic list) result
(** Read and strictly decode one lock file. Diagnostic paths are exactly
    [path]. *)

val find_in_repo : repo:string -> (t option, Model.diagnostic list) result
(** [find_in_repo ~repo] reads [repo/nomia.lock] when it exists and
    returns [Ok None] otherwise. Diagnostics name the relative lock file, not
    the absolute repository path. *)

val to_yaml_string : t -> string
(** Deterministic YAML spelling of a lock, useful for the reviewable re-pin
    operation. Optional fields appear only when present. *)
