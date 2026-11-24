(** An immutable, content-addressed release manifest.

    A release is a pure function of the inputs named by the manifest: the
    policy source bytes, the adapter ids and versions, the selected project,
    the ordered repository fragment paths and digests, the selected role, the
    active exception ids, and the policy compiler version. No wall-clock time,
    absolute machine path, or rendered artifact byte participates in the input
    digest. *)

type t

val build :
  ?client:string ->
  ?role:string ->
  ?project:string ->
  ?inherit_root:bool ->
  ?fragments:(string * string) list ->
  ?exceptions:string list ->
  ?artifacts:(string * string) list ->
  Model.source ->
  t
(** [build source] records the source files, rule metadata, skills, roles,
    adapters, exception ids, and artifacts in canonical order.

    [client] is the selected adapter id (different adapters are different
    releases), [role] is the selected role name (if any), [project] is the
    selected monorepo project, [inherit_root] marks a project-only overlay,
    [fragments] is the ordered
    [(repository-relative path, digest)] fragment chain, [exceptions] defaults
    to the loaded source's exception ids, and [artifacts] are
    [(relative path, content)] pairs that are hashed but do not affect the
    input digest.

    Raises [Invalid_argument] for an empty or absolute artifact path. *)

val manifest_json : t -> Yojson.Safe.t
(** The complete manifest, including its [digest] field. Keys are safe to
    serialize with {!Canonical.to_string}. *)

val to_string : t -> string
(** Compact canonical manifest bytes. Two builds from identical inputs produce
    identical bytes. *)

val digest : t -> string
(** The overall input digest, ["sha256:" ^ 64 hex]. *)

val write_manifest : dir:string -> t -> (unit, string) result
(** [write_manifest ~dir t] creates [dir] when needed and atomically writes
    [dir/manifest.json] (canonical bytes plus one trailing newline). *)

val source_digest : Model.source -> string
(** ["sha256:" ^ 64 hex] over the sorted [(relative path, sha256)] source file
    table. Independent of filesystem enumeration order and absolute root. *)
