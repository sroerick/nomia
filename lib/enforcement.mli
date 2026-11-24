(** Normalized enforcement decision seam for trusted host wrappers.

    The model never authorizes itself: only a trusted wrapper turns a
    prospective action into a {!request} and calls {!decide}. The decision is
    pure over the composed policy and the request. Deny wins over ask over
    allow, unsupported adapter controls stay visible, and the result never
    contains prompt text or secret values. *)

type decision = Allow | Deny | Ask

type request = {
  action : string;  (** Normalized action kind, such as [fs.read]. *)
  tool : string;  (** Host tool name, for logs and host-side correlation. *)
  role : string option;
  repo : string option;
  project : string option;  (** Repository-relative project context. *)
  policy_digest : string option;
      (** When supplied, must equal {!Compose.digest}. *)
  paths : string list;  (** Typed filesystem paths supplied by the host. *)
  network_hosts : string list;  (** Typed network hosts supplied by the host. *)
}

type result = {
  decision : decision;
  rule_ids : string list;  (** Sorted and unique. *)
  obligations : string list;
      (** Sorted and unique host-side obligations, such as evidence keys or
          a request for missing path/host context. *)
  required_controls : string list;  (** Sorted and unique. *)
  unsupported_controls : string list;  (** Sorted and unique. *)
  adapter : string;
  project : string option;
  policy_digest : string;
}

val decide :
  Compose.t ->
  request ->
  (result, Model.diagnostic list) Stdlib.result
(** Compute one decision. A policy-digest mismatch is a diagnostic and yields
    no decision. A prohibition denies at this trusted seam even when the
    selected adapter cannot supply one of its controls; the unsupported
    control is still named. *)

val request_of_json :
  Yojson.Safe.t -> (request, Model.diagnostic list) Stdlib.result
(** Decode a strict JSON request object. Unknown fields, wrong types, and a
    missing or empty [action] are diagnostics. *)

val request_of_string : string -> (request, Model.diagnostic list) Stdlib.result
(** Parse and decode one JSON request. *)

val decision_name : decision -> string
(** [allow], [deny], or [ask]. *)

val to_json : result -> Yojson.Safe.t
(** Canonical JSON document for one decision. *)

val to_string : result -> string
(** Canonical compact JSON; deterministic for identical inputs. *)

val human_line : result -> string
(** One-line human rendering of the same decision. *)
