(** Deterministic composition of one effective PolicySet.

    Resolution order inside organization-supplied context is host controls,
    organization rules, the trusted repository fragment, then the selected
    role. A lower layer may add or narrow; it may not relax a final
    organization rule. Explain and render consume this same resolution.

    All recorded paths are relative to their source root or repository. No
    absolute machine path, wall-clock value other than the explicit [today],
    or filesystem enumeration order participates in the result. *)

type origin = Organization | Repository | Role

type fragment_scope = Root | Project

type support = [ `Supported | `Partial | `Unsupported ]

type resolved_rule = {
  rule : Model.rule;
  origin : origin;
  active_exception : Model.exception_ option;
  support : support;
  unsupported_controls : string list;
      (** Controls the selected adapter cannot provide. They remain visible as
          warnings and never become enforcement claims. *)
}

type fragment = {
  scope : fragment_scope;  (** Repository-root or selected-project fragment. *)
  path : string;  (** Repository-relative fragment path. *)
  digest : string;  (** ["sha256:" ^ 64 hex]. *)
  prose : string;
  rules : Model.rule list;
}

type t = {
  source : Model.source;
  release : Release.t;
  digest : string;  (** [Release.digest] of the selected inputs. *)
  source_digest : string;
  adapter : Model.adapter;
  role : Model.role option;
  project : string option;  (** Repository-relative selected project, if any. *)
  fragments : fragment list;
      (** Ordered root fragment, then selected project fragment. *)
  lock : Lock.t option;
  rules : resolved_rule list;
      (** Organization rules by id, then repository rules, then role
          additions. Each group is deterministic. *)
  skills : Model.skill list;  (** By name. *)
  exceptions : Model.exception_ list;  (** Active only, by id. *)
  warnings : string list;  (** Sorted and deduplicated. *)
  today : string;  (** Calendar date used for exception expiry reasoning. *)
}

val resolve :
  ?repo:string ->
  ?role:string ->
  ?client:string ->
  ?project:string ->
  ?ignore_lock:bool ->
  ?today:string ->
  Model.source ->
  (t, Model.diagnostic list) result
(** Resolve one PolicySet from an already loaded source. [repo] selects a
    repository context for its lock and fragments; [project] selects one
    repository-relative project and adds that project's fragment after the
    root fragment; [role] and [client] override lock values. An unknown client,
    role, or project is a diagnostic. A lock whose release, compiler, or
    source digest does not match is a re-pin diagnostic. [ignore_lock] is for
    the explicit sync re-pin path, which must resolve the candidate without
    letting the old lock veto it. *)

val resolve_root :
  ?repo:string ->
  ?role:string ->
  ?client:string ->
  ?project:string ->
  ?today:string ->
  root:string ->
  unit ->
  (t, Model.diagnostic list) result
(** Load and validate the source at [root], then {!resolve}. [today] defaults
    to [NOMIA_TODAY] when set, otherwise the UTC date. *)

val explain_json : t -> Yojson.Safe.t
(** Deterministic explain document, schema [nomia-explain/v2]. Serialize
    with {!Canonical.to_string} for canonical bytes. *)

val explain_text : t -> string
(** Compact human rendering of exactly the same PolicySet. It never performs a
    second merge. *)
