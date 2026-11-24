(** Managed target layout and content-addressed release cache.

    A managed target owns exactly one [.nomia] directory:

    {v
      <target>/.nomia/releases/<digest>/...  immutable rendered release
      <target>/.nomia/current -> releases/<digest>
      <target>/.nomia/state.json
      <target>/AGENTS.md -> .nomia/current/AGENTS.md
    v}

    Only artifact bytes and canonical JSON cross this seam. Errors name
    relative files, never private absolute machine paths. *)

type target = {
  root : string;
  managed : string;
  releases : string;
  current : string;
  state_path : string;
}

val target_of_root : string -> target
(** [target_of_root root] describes [root]'s managed layout without touching
    the filesystem. *)

val managed_dir : string
(** The managed directory name, [".nomia"]. *)

val client_host_root : unit -> string
(** [$XDG_CONFIG_HOME/nomia], defaulting to
    [$HOME/.config/nomia]. *)

val client_target : client:string -> string
(** [client_host_root ()/clients/client], the user-level managed target. It is
    never a native client configuration path. *)

val valid_digest : string -> bool
(** [sha256:] plus exactly 64 lowercase hex characters. *)

val safe_relative_path : string -> bool
(** Non-empty, relative, no backslash, and no [.] or [..] component. *)

val cache_root : unit -> string
(** [$XDG_CACHE_HOME/nomia], defaulting to [$HOME/.cache/nomia]. *)

type state = {
  schema : string;
  release : string;
  digest : string;
  source_digest : string;
  client : string;
  role : string option;
  compiler : string;
  entrypoints : string list;  (** Sorted, relative to the target root. *)
}

val state_schema : string
(** ["nomia-state/v1"]. *)

val state_to_json : state -> Yojson.Safe.t
val state_to_string : state -> string
(** Canonical JSON plus one trailing newline. Contains no path or time. *)

val state_of_json : Yojson.Safe.t -> (state, string) result
val read_state : target -> (state option, string) result
(** [Ok None] when [state.json] does not exist. *)

val write_state : target -> state -> (unit, string) result
(** Write [state.json] through a temporary file and rename. *)

val state_next_path : target -> string
val write_state_next : target -> state -> (unit, string) result
(** Stage the state beside [state.json] before [current] is swapped. *)

val promote_state_next : target -> (unit, string) result
(** Rename [state.next.json] over [state.json] after a successful swap. *)

val discard_state_next : target -> unit
(** Remove an unselected staged state, if any. *)

val read_state_next : target -> (state option, string) result
(** The staged state, if any. *)

val effective_state : target -> (state option, string) result
(** [current] is authoritative. When [state.next.json] selects the current
    release and [state.json] is missing or older, return the staged state. This
    is the crash-recovery view after a swap but before the state rename. *)

val recover_state : target -> (unit, string) result
(** Promote a staged state that matches [current], discard one that does not. *)

val release_dir : target -> digest:string -> string
val staging_path : target -> string

val file_exists : string -> bool
val directory_exists : string -> bool
val read_file : string -> (string, string) result
val write_file : path:string -> mode:int -> string -> (unit, string) result
(** Create parents, write bytes, flush, fsync, close, and set the mode. *)

val atomic_write : path:string -> mode:int -> string -> (unit, string) result
(** Write a sibling temporary, fsync it, then rename over [path]. *)

val mkdir_p : string -> (unit, string) result
val remove_tree : string -> unit
val list_dir : string -> string list
(** Sorted names, excluding [.] and [..]. A missing directory is empty. *)

val read_symlink : string -> (string option, string) result
(** [Ok None] when the path is absent or is not a symbolic link. *)

val current_digest : target -> (string option, string) result
(** The release digest selected by [current], or [None] when [current] is
    absent, is not a symlink, or does not name [releases/<digest>]. *)

val verify_tree : root:string -> digest:string -> (unit, string) result
(** Verify that [root] contains exactly the release manifest's artifacts plus
    [.nomia/manifest.json], with every recorded sha256 intact. A
    symlink, missing file, changed byte, or unexpected file is an error naming
    the relative path. *)

val verify_release : target -> digest:string -> (unit, string) result
(** [verify_tree] over [releases/<digest>]. *)

val entrypoint_link : entrypoint:string -> string
(** The stable relative symlink text for [entrypoint], for example
    [.nomia/current/AGENTS.md] and
    [../.nomia/current/.opencode/skills]. *)

val ensure_entrypoints :
  target -> entrypoints:string list -> (unit, string) result
(** Create or repair only managed entrypoint symlinks. A regular file or
    unmanaged directory at an entrypoint is an error and is never overwritten. *)

val swap_current : target -> digest:string -> (unit, string) result
(** Atomically replace [current] with a symlink to [releases/<digest>]. *)

val prune_releases :
  target -> current:string -> keep:int -> (unit, string) result
(** Keep [current] and the [keep] most recently modified other releases.
    Hidden staging directories are never treated as releases. *)

type cached = {
  manifest : Yojson.Safe.t;
  explain : Yojson.Safe.t;
  artifacts : (string * string * int) list;
      (** [(relative path, bytes, mode)], sorted by path. *)
}

val cache_release :
  digest:string ->
  manifest:Yojson.Safe.t ->
  explain:Yojson.Safe.t ->
  artifacts:(string * string * int) list ->
  (unit, string) result
(** Content-addressed cache write. An existing digest directory is never
    rewritten; a complete new release is staged and renamed into place. *)

val load_cached : digest:string -> (cached, string) result
(** Load one cached release only after the manifest digest, every artifact
    hash, and the explain copy agree. *)

val cache_digests : unit -> string list
(** Sorted cached release digests. *)

val cache_manifest : digest:string -> (Yojson.Safe.t, string) result

val load_installed : root:string -> digest:string -> (cached, string) result
(** Load an installed managed release from [root] after verifying its tree.
    This is the fallback when the content-addressed cache is absent. *)
