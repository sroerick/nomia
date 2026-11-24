(** Atomic synchronization and drift diagnosis for managed targets.

    Sync validates a complete rendered candidate before it touches the target,
    then stages, verifies, and atomically selects it. Doctor reports one
    state, the installed/source/cached digests, entrypoints, and honest
    enforcement coverage. Neither operation uses the network, a provider, a
    credential, or a native client configuration path. *)

type error = Diagnostics of Model.diagnostic list | Message of string

type sync_result = {
  state : Store.state;
  changed : bool;  (** [current] was swapped by this run. *)
  lock_updated : bool;
  warnings : string list;
}

val sync :
  repo:string option ->
  client:string option ->
  role:string option ->
  project:string option ->
  source_root:string ->
  update_pin:bool ->
  (sync_result, error) result
(** [sync ~repo ~client ~role ~project ~source_root ~update_pin] installs one
    selected release. Exactly one of [repo] and [client] must select the
    target. A repository without [nomia.lock] refuses until [update_pin] writes
    the reviewable pin. A pin mismatch likewise refuses without [update_pin].
    With [repo], [project] installs the project-scoped release under
    [repo/project] while the single pin remains at [repo/nomia.lock].
    User-level sync writes only under
    [$XDG_CONFIG_HOME/nomia/clients/<client>].

    The test-only environment variable [NOMIA_FAULT=before-swap]
    returns an error after staging and verification but before [current] is
    swapped. The previous complete target stays usable. *)

type doctor_state =
  | Current
  | Stale
  | Missing
  | Unmanaged
  | Hand_edited
  | Partial
  | Offline_last_known_good
  | Source_unavailable

type coverage = {
  rules : int;  (** Effective rules carrying enforcement metadata. *)
  supported : int;
  partial : int;
  unsupported : int;
  unsupported_rules : string list;
  unsupported_controls : string list;
}

type issue = { code : string; message : string }

type report = {
  state : doctor_state;
  target_kind : string;  (** ["repo"] or ["client"]. *)
  client : string option;
  role : string option;
  installed_digest : string option;
  installed_release : string option;
  installed_source_digest : string option;
  source_available : bool;
  source_digest : string option;
  source_files_digest : string option;
  source_release : string option;
  project : string option;
  cached_digests : string list;
  entrypoints : string list;
  coverage : coverage;
  issues : issue list;
  warning : string option;
}

val doctor :
  repo:string option ->
  project:string option ->
  role:string option ->
  client:string option ->
  source_root:string option ->
  (report, error) result
(** [doctor ~repo ~project ~role ~client ~source_root] reports one state. When
    [repo] and [project] are both set, the managed target is [repo/project] and
    the pin remains at [repo/nomia.lock]. [source_root = None]
    means discovery failed or the source is absent; an intact installed or
    cached release is then reported as [Offline_last_known_good] rather than
    inventing an empty policy. *)

val doctor_state_name : doctor_state -> string
val doctor_to_json : report -> Yojson.Safe.t
(** Canonical doctor JSON. It contains digests, releases, relative
    entrypoints, coverage, and issues; never an absolute path or timestamp. *)

val doctor_text : report -> string
val doctor_exit : report -> int
(** 0 for current, partial, and offline-last-known-good; 1 for stale, missing,
    unmanaged, and hand-edited; 2 for an unavailable source with no cache. *)
