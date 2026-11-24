(** Canonical JSON bytes and content digests.

    A canonical form is the same for every in-memory spelling of the same
    JSON value: object keys are sorted by byte order at every depth, lists
    keep their order, separators are compact, and floating-point values use
    one deterministic rendering. Non-finite floats are rejected because they
    have no standard JSON spelling. *)

val sort : Yojson.Safe.t -> Yojson.Safe.t
(** [sort json] recursively sorts every object's keys by byte order.
    Lists are left in place. The operation is idempotent. *)

val to_string : Yojson.Safe.t -> string
(** Compact canonical JSON: [sort]d keys, no insignificant whitespace, and a
    stable string escape and number rendering. Raises [Invalid_argument] when
    the value contains NaN or either infinity. *)

val sha256_hex : string -> string
(** Lowercase hex sha256 of the string. *)

val sha256_file : string -> string
(** Lowercase hex sha256 of the file's bytes. Filesystem errors propagate as
    [Sys_error]. *)

val length_prefixed : string -> string
(** [length_prefixed s] is an unsigned 64-bit big-endian byte length followed
    by [s]. Prefixing every segment makes concatenated frames unambiguous:
    [length_prefixed "ab" ^ length_prefixed "c"] differs from
    [length_prefixed "a" ^ length_prefixed "bc"]. *)

val manifest_digest : Yojson.Safe.t -> string
(** [manifest_digest json] is ["sha256:" ^ hex] over

    {v "nomia-manifest-v1\000" ^ length_prefixed (to_string json) v}

    The domain separator and the length prefix make the digest independent of
    how the caller concatenates or frames its inputs. *)
