(** Normalized enforcement decision seam.

    A trusted host wrapper turns one prospective tool action into a
    {!request} and asks {!decide} for [allow | deny | ask]. The decision is a
    pure function of the resolved {!Compose.t}, the request, and the adapter
    capability matrix. No provider, credential, network, prompt, or filesystem
    access happens here, and no prompt text or secret value can appear in the
    result.

    Rules match by normalized action first. A rule with guards additionally
    needs one applicable guard to match: the guard must name the request action
    (or [*]), and any guard paths/hosts must match the request's typed paths or
    network hosts. A path- or host-guarded rule that matches on action but has
    no supplied context yields [Ask] with a context obligation rather than a
    silent allow or a blanket deny.

    Decision precedence is deny over ask over allow. Unsupported adapter
    controls are reported, never converted into an enforcement claim; a
    prohibition still denies at this trusted seam (fail closed) while naming
    the control the adapter cannot provide. *)

open Model

type decision = Allow | Deny | Ask

type request = {
  action : string;
  tool : string;
  role : string option;
  repo : string option;
  project : string option;
  policy_digest : string option;
  paths : string list;
  network_hosts : string list;
}

type result = {
  decision : decision;
  rule_ids : string list;  (** Sorted and unique. *)
  obligations : string list;  (** Sorted and unique. *)
  required_controls : string list;  (** Sorted and unique. *)
  unsupported_controls : string list;  (** Sorted and unique. *)
  adapter : string;
  project : string option;
  policy_digest : string;
}

let decision_name = function
  | Allow -> "allow"
  | Deny -> "deny"
  | Ask -> "ask"

let decision_rank = function Allow -> 0 | Ask -> 1 | Deny -> 2

let promote current next =
  if decision_rank next > decision_rank current then next else current

let distinct values = List.sort_uniq String.compare values

let strings values = `List (List.map (fun value -> `String value) values)

let diagnostic code message : diagnostic =
  { code; path = "request"; line = None; message }

(* ------------------------------------------------------------------ *)
(* Typed request decoding                                             *)
(* ------------------------------------------------------------------ *)

let known_fields =
  [
    "action";
    "tool";
    "role";
    "repo";
    "project";
    "policy_digest";
    "paths";
    "network_hosts";
  ]

let compare_diagnostic left right =
  let by_code = String.compare left.code right.code in
  if by_code <> 0 then by_code else String.compare left.message right.message

let normalize_diagnostics diagnostics =
  List.sort_uniq compare_diagnostic diagnostics

let request_of_json json =
  match json with
  | `Assoc fields ->
      let errors = ref [] in
      let add code message = errors := diagnostic code message :: !errors in
      List.iter
        (fun (key, _) ->
          if not (List.mem key known_fields) then
            add "enforcement.request.unknown-field"
              (Printf.sprintf "unknown request field '%s'" key))
        fields;
      let get_string name =
        match List.assoc_opt name fields with
        | Some (`String value) -> Some value
        | Some `Null | None -> None
        | Some _ ->
            add "enforcement.request.type"
              (Printf.sprintf "request field '%s' must be a string" name);
            None
      in
      let get_string_list name =
        match List.assoc_opt name fields with
        | Some (`List items) ->
            List.filter_map
              (function
                | `String value -> Some value
                | _ ->
                    add "enforcement.request.type"
                      (Printf.sprintf
                         "every item of request field '%s' must be a string" name);
                    None)
              items
        | Some `Null | None -> []
        | Some _ ->
            add "enforcement.request.type"
              (Printf.sprintf
                 "request field '%s' must be a list of strings" name);
            []
      in
      let action = get_string "action" |> Option.value ~default:"" in
      if String.trim action = "" then
        add "enforcement.request.action"
          "request must name a non-empty normalized action";
      let tool = get_string "tool" |> Option.value ~default:"" in
      let role = get_string "role" in
      let repo = get_string "repo" in
      let project = get_string "project" in
      let policy_digest = get_string "policy_digest" in
      let paths = get_string_list "paths" in
      let network_hosts = get_string_list "network_hosts" in
      if !errors <> [] then Error (normalize_diagnostics !errors)
      else
        Ok
          {
            action = String.trim action;
            tool;
            role;
            repo;
            project;
            policy_digest;
            paths;
            network_hosts;
          }
  | _ ->
      Error
        [
          diagnostic "enforcement.request.shape"
            "request must be a JSON object";
        ]

let request_of_string text =
  try request_of_json (Yojson.Safe.from_string text)
  with Yojson.Json_error message ->
    Error
      [
        diagnostic "enforcement.request.json"
          (Printf.sprintf "cannot parse request JSON: %s" message);
      ]

(* ------------------------------------------------------------------ *)
(* Guard and rule matching                                            *)
(* ------------------------------------------------------------------ *)

(* [*] matches any run of non-slash characters; [**] crosses slashes; [?]
   matches one non-slash character. A [**/] prefix also matches zero
   directories, so [**/.env] matches both [.env] and [a/.env]. *)
let glob_match pattern text =
  let pattern_length = String.length pattern in
  let text_length = String.length text in
  let rec matches pattern_index text_index =
    if pattern_index = pattern_length then text_index = text_length
    else
      match pattern.[pattern_index] with
      | '*' when pattern_index + 1 < pattern_length
                 && pattern.[pattern_index + 1] = '*' ->
          let rec skip_stars index =
            if index < pattern_length && pattern.[index] = '*' then
              skip_stars (index + 1)
            else index
          in
          let next = skip_stars (pattern_index + 2) in
          let rec consume index =
            if matches next index then true
            else if
              next < pattern_length && pattern.[next] = '/'
              && matches (next + 1) index
            then true
            else if index < text_length then consume (index + 1)
            else false
          in
          consume text_index
      | '*' ->
          let rec consume index =
            if matches (pattern_index + 1) index then true
            else if index < text_length && text.[index] <> '/' then
              consume (index + 1)
            else false
          in
          consume text_index
      | '?' ->
          text_index < text_length
          && text.[text_index] <> '/'
          && matches (pattern_index + 1) (text_index + 1)
      | character ->
          text_index < text_length
          && text.[text_index] = character
          && matches (pattern_index + 1) (text_index + 1)
  in
  matches 0 0

type guard_result =
  | Guard_match
  | Guard_mismatch
  | Guard_needs_context of string list

let eval_guard (request : request) (guard : guard) =
  if guard.action <> request.action && guard.action <> "*" then Guard_mismatch
  else
    let path_check =
      if guard.paths = [] then `Ok
      else if request.paths = [] then `Missing "provide-path-context"
      else if
        List.exists
          (fun pattern ->
            List.exists (fun path -> glob_match pattern path) request.paths)
          guard.paths
      then `Ok
      else `Mismatch
    in
    let host_check =
      if guard.hosts = [] then `Ok
      else if request.network_hosts = [] then `Missing "provide-host-context"
      else if
        List.exists
          (fun pattern ->
            List.exists
              (fun host -> glob_match pattern host)
              request.network_hosts)
          guard.hosts
      then `Ok
      else `Mismatch
    in
    match (path_check, host_check) with
    | `Mismatch, _ | _, `Mismatch -> Guard_mismatch
    | `Missing left, `Missing right ->
        Guard_needs_context (distinct [ left; right ])
    | `Missing missing, `Ok | `Ok, `Missing missing ->
        Guard_needs_context [ missing ]
    | `Ok, `Ok -> Guard_match

type rule_result = Rule_match | Rule_mismatch | Rule_needs_context of string list

let eval_rule (request : request) (rule : rule) =
  if
    not
      (List.exists
         (fun action -> action = request.action || action = "*")
         rule.actions)
  then Rule_mismatch
  else if rule.guards = [] then Rule_match
  else
    let applicable =
      List.filter
        (fun (guard : guard) ->
          guard.action = request.action || guard.action = "*")
        rule.guards
    in
    if applicable = [] then Rule_mismatch
    else
      let results = List.map (eval_guard request) applicable in
      if List.mem Guard_match results then Rule_match
      else
        let missing =
          List.concat_map
            (function
              | Guard_needs_context obligations -> obligations
              | Guard_match | Guard_mismatch -> [])
            results
        in
        if missing = [] then Rule_mismatch
        else Rule_needs_context (distinct missing)

(* ------------------------------------------------------------------ *)
(* Decision                                                           *)
(* ------------------------------------------------------------------ *)

let controls_of (rule : rule) =
  match rule.enforcement with Some enforcement -> enforcement.controls | None -> []

let class_decision (rule : rule) =
  match rule.enforcement with
  | None -> Allow
  | Some enforcement -> (
      match enforcement.cls with
      | Prohibition -> Deny
      | Consent -> Ask
      | Gate -> Allow
      | Advisory -> Allow)

let obligations_of (rule : rule) =
  match rule.enforcement with
  | None -> []
  | Some enforcement -> (
      match enforcement.cls with
      | Prohibition -> [ "do-not-proceed" ]
      | Consent -> [ Option.value ~default:"request-consent" rule.key ]
      | Gate -> [ Option.value ~default:"provide-evidence" rule.key ]
      | Advisory ->
          if rule.final then [ "instruction:" ^ rule.id ] else [])

type accumulator = {
  current : decision;
  ids : string list;
  obligations : string list;
  controls : string list;
  unsupported : string list;
}

let empty_accumulator =
  {
    current = Allow;
    ids = [];
    obligations = [];
    controls = [];
    unsupported = [];
  }

let add_common accumulator (resolved : Compose.resolved_rule) =
  {
    accumulator with
    ids = resolved.rule.id :: accumulator.ids;
    controls = controls_of resolved.rule @ accumulator.controls;
    unsupported = resolved.unsupported_controls @ accumulator.unsupported;
  }

let decide (policy_set : Compose.t) (request : request) =
  let context_errors =
    (match request.project with
    | Some supplied when policy_set.Compose.project <> Some supplied ->
        [
          diagnostic "enforcement.project"
            (Printf.sprintf
               "request project '%s' does not match the composed project '%s'"
               supplied
               (Option.value policy_set.Compose.project ~default:"(none)"));
        ]
    | _ -> [])
    @
    (match request.policy_digest with
    | Some supplied when supplied <> policy_set.Compose.digest ->
        [
          diagnostic "enforcement.policy-digest"
            (Printf.sprintf
               "request policy digest '%s' does not match the composed policy \
                digest '%s'"
               supplied policy_set.Compose.digest);
        ]
    | _ -> [])
  in
  match context_errors with
  | _ :: _ -> Error context_errors
  | [] ->
      let accumulator =
        List.fold_left
          (fun accumulator (resolved : Compose.resolved_rule) ->
            match eval_rule request resolved.rule with
            | Rule_mismatch -> accumulator
            | Rule_match ->
                let accumulator = add_common accumulator resolved in
                {
                  accumulator with
                  current =
                    promote accumulator.current (class_decision resolved.rule);
                  obligations =
                    obligations_of resolved.rule @ accumulator.obligations
                }
            | Rule_needs_context obligations ->
                let accumulator = add_common accumulator resolved in
                {
                  accumulator with
                  current = promote accumulator.current Ask;
                  obligations = obligations @ accumulator.obligations;
                })
          empty_accumulator policy_set.Compose.rules
      in
      Ok
        {
          decision = accumulator.current;
          rule_ids = distinct accumulator.ids;
          obligations = distinct accumulator.obligations;
          required_controls = distinct accumulator.controls;
          unsupported_controls = distinct accumulator.unsupported;
          adapter = policy_set.Compose.adapter.Model.id;
          project = policy_set.Compose.project;
          policy_digest = policy_set.Compose.digest;
        }

(* ------------------------------------------------------------------ *)
(* Output                                                             *)
(* ------------------------------------------------------------------ *)

let to_json result =
  `Assoc
    [
      ("adapter", `String result.adapter);
      ("decision", `String (decision_name result.decision));
      ("obligations", strings result.obligations);
      ("policy_digest", `String result.policy_digest);
      ( "project",
        match result.project with None -> `Null | Some value -> `String value );
      ("required_controls", strings result.required_controls);
      ("rule_ids", strings result.rule_ids);
      ("unsupported_controls", strings result.unsupported_controls);
    ]

let to_string result = Canonical.to_string (to_json result)

let joined values =
  match values with [] -> "-" | _ -> String.concat "," values

let human_line result =
  Printf.sprintf "%s adapter=%s project=%s rules=%s controls=%s \
                  unsupported=%s obligations=%s"
    (decision_name result.decision)
    result.adapter
    (Option.value result.project ~default:"-")
    (joined result.rule_ids) (joined result.required_controls)
    (joined result.unsupported_controls) (joined result.obligations)
