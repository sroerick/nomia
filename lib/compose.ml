(** Deterministic composition of one effective PolicySet. *)

open Model

type origin = Organization | Repository | Role

type fragment_scope = Root | Project

type support = [ `Supported | `Partial | `Unsupported ]

type resolved_rule = {
  rule : Model.rule;
  origin : origin;
  active_exception : Model.exception_ option;
  support : support;
  unsupported_controls : string list;
}

type fragment = {
  scope : fragment_scope;
  path : string;
  digest : string;
  prose : string;
  rules : Model.rule list;
}

type t = {
  source : Model.source;
  release : Release.t;
  digest : string;
  source_digest : string;
  adapter : Model.adapter;
  role : Model.role option;
  project : string option;
  fragments : fragment list;  (** Root fragment, then project fragment. *)
  lock : Lock.t option;
  rules : resolved_rule list;
  skills : Model.skill list;
  exceptions : Model.exception_ list;
  warnings : string list;
  today : string;
}

(* ------------------------------------------------------------------ *)
(* Small pure helpers                                                  *)
(* ------------------------------------------------------------------ *)

let diag code path line message : Model.diagnostic =
  { code; path; line; message }

let contains_sub hay needle =
  let hay_len = String.length hay and needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec loop index =
      if index + needle_len > hay_len then false
      else if String.sub hay index needle_len = needle then true
      else loop (index + 1)
    in
    loop 0

let ends_with ~suffix value =
  let value_len = String.length value and suffix_len = String.length suffix in
  value_len >= suffix_len
  && String.sub value (value_len - suffix_len) suffix_len = suffix

let compare_diagnostic left right =
  match String.compare left.Model.path right.Model.path with
  | 0 -> (
      match (left.Model.line, right.Model.line) with
      | None, None -> String.compare left.Model.code right.Model.code
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some left_line, Some right_line ->
          let by_line = Int.compare left_line right_line in
          if by_line <> 0 then by_line
          else String.compare left.Model.code right.Model.code)
  | order -> order

let normalize_diagnostics diagnostics =
  List.sort_uniq
    (fun left right ->
      let order = compare_diagnostic left right in
      if order <> 0 then order
      else String.compare left.Model.message right.Model.message)
    diagnostics
  |> List.sort compare_diagnostic

let strength_name = function Must -> "must" | Should -> "should" | May -> "may"

let kind_name = function
  | Instruction -> "instruction"
  | Procedure -> "procedure"
  | Knowledge -> "knowledge"
  | Capability -> "capability"

let class_name = function
  | Advisory -> "advisory"
  | Gate -> "gate"
  | Consent -> "consent"
  | Prohibition -> "prohibition"

let layer_name = function
  | Model.Organization -> "organization"
  | Model.Repository -> "repository"
  | Model.Role -> "role"

let origin_name = function
  | Organization -> "organization"
  | Repository -> "repository"
  | Role -> "role"

let origin_rank = function Organization -> 0 | Repository -> 1 | Role -> 2

let support_name = function
  | `Supported -> "supported"
  | `Partial -> "partial"
  | `Unsupported -> "unsupported"

let compare_rule_by_id (left : rule) (right : rule) =
  let by_id = String.compare left.id right.id in
  if by_id <> 0 then by_id
  else
    let by_revision = Int.compare left.revision right.revision in
    if by_revision <> 0 then by_revision
    else
      let by_source = String.compare left.source right.source in
      if by_source <> 0 then by_source else Int.compare left.line right.line

let compare_exception (left : exception_) (right : exception_) =
  let by_id = String.compare left.id right.id in
  if by_id <> 0 then by_id else String.compare left.rule right.rule

let parse_supersedes value =
  match String.rindex_opt value '@' with
  | None -> None
  | Some index ->
      let id = String.sub value 0 index in
      let revision_text =
        String.sub value (index + 1) (String.length value - index - 1)
      in
      if id = "" || revision_text = "" then None
      else
        match int_of_string_opt revision_text with
        | Some revision when revision >= 0 -> Some (id, revision)
        | _ -> None

let utc_today () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let today_value ?today () =
  match today with
  | Some value -> String.trim value
  | None -> (
      match Sys.getenv_opt "NOMIA_TODAY" with
      | Some value when String.trim value <> "" -> String.trim value
      | _ -> utc_today ())

let read_text path =
  try Some (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error _ -> None

let file_exists path = try Sys.file_exists path with Sys_error _ -> false

let is_generated text =
  contains_sub text "<!-- nomia:generated"
  || contains_sub text "Generated by agentctl"

(* ------------------------------------------------------------------ *)
(* Fragment selection                                                  *)
(* ------------------------------------------------------------------ *)

let path_has_parent_or_empty value =
  String.split_on_char '/' value
  |> List.exists (fun part -> part = "" || part = "." || part = "..")

let valid_relative_path value =
  String.trim value <> ""
  && not (String.contains value '\000')
  && not (String.contains value '\\')
  && Filename.is_relative value
  && not (path_has_parent_or_empty value)

let normalize_project value =
  let value = String.trim value in
  if value = "" || value = "." then Ok None
  else if not (valid_relative_path value) then
    Error
      (diag "compose.project.path" value None
         "project must be a non-empty path relative to the repository root \
          with no '.', '..', empty, or backslash components")
  else Ok (Some value)

let load_fragment ~repo ~scope ~add ~warn rel =
  match Source.load_fragment_rel ~root:repo ~rel with
  | Error diagnostics ->
      List.iter add diagnostics;
      None
  | Ok (rules, prose, files) ->
      let digest =
        match List.assoc_opt rel files with
        | Some hash -> "sha256:" ^ hash
        | None -> "sha256:"
      in
      Some { scope; path = rel; digest; prose; rules }

let select_fragment_at ~repo ~scope ~add ~warn directory =
  let dedicated = Filename.concat directory "AGENTS.fragment.md" in
  let fallback = Filename.concat directory "AGENTS.md" in
  let rel name =
    if directory = repo then name
    else
      let prefix =
        String.sub directory (String.length repo)
          (String.length directory - String.length repo)
      in
      let prefix =
        if String.length prefix > 0 && prefix.[0] = '/' then
          String.sub prefix 1 (String.length prefix - 1)
        else prefix
      in
      prefix ^ "/" ^ name
  in
  if file_exists dedicated then load_fragment ~repo ~scope ~add ~warn (rel "AGENTS.fragment.md")
  else if file_exists fallback then
    match read_text fallback with
    | None ->
        add
          (diag "source.read" (rel "AGENTS.md") None
             "cannot read repository AGENTS.md");
        None
    | Some text ->
        if is_generated text then begin
          warn
            (Printf.sprintf
               "repository %s looks generated by agentctl; skipped as a \
                fragment"
               (rel "AGENTS.md"));
          None
        end
        else load_fragment ~repo ~scope ~add ~warn (rel "AGENTS.md")
  else None

let select_fragments ~repo ~project ~(lock : Lock.t option) ~add ~warn =
  match repo with
  | None -> []
  | Some repo ->
      let root =
        match Option.bind lock (fun lock -> lock.Lock.fragment) with
        | Some rel ->
            if not (valid_relative_path rel) then begin
              add
                (diag "compose.fragment.path" rel None
                   "lock fragment path must be relative to the repository root \
                    with no '.', '..', empty, or backslash components");
              None
            end
            else if not (file_exists (Filename.concat repo rel)) then begin
              add
                (diag "compose.fragment.missing" rel None
                   (Printf.sprintf
                      "locked repository fragment '%s' is missing; restore it \
                       or re-pin with an explicit reviewed change"
                      rel));
              None
            end
            else load_fragment ~repo ~scope:Root ~add ~warn rel
        | None -> select_fragment_at ~repo ~scope:Root ~add ~warn repo
      in
      let project_fragment =
        match project with
        | None -> None
        | Some project ->
            let directory = Filename.concat repo project in
            if not (Sys.file_exists directory && Sys.is_directory directory) then begin
              add
                (diag "compose.project.missing" project None
                   (Printf.sprintf
                      "project directory '%s' does not exist under the \
                       repository root"
                      project));
              None
            end
            else
              select_fragment_at ~repo ~scope:Project ~add ~warn directory
      in
      List.filter_map Fun.id [ root; project_fragment ]

(* ------------------------------------------------------------------ *)
(* Exceptions and repository scope                                     *)
(* ------------------------------------------------------------------ *)

let normalize_repo value =
  let buffer = Bytes.of_string value in
  let length = ref (String.length value) in
  while !length > 1 && Bytes.get buffer (!length - 1) = '/' do
    decr length
  done;
  Bytes.sub_string buffer 0 !length

let repo_scope_matches ~repo pattern =
  let repo = normalize_repo repo and pattern = normalize_repo pattern in
  let basename = Filename.basename repo in
  repo = pattern || basename = pattern
  || ends_with ~suffix:("/" ^ pattern) repo

let exception_is_active ~repo ~project ~today (exception_ : exception_) =
  String.compare exception_.expires today > 0
  &&
  match String.index_opt exception_.scope ':' with
  | None -> false
  | Some index ->
      let prefix = String.sub exception_.scope 0 index in
      let target =
        String.sub exception_.scope (index + 1)
          (String.length exception_.scope - index - 1)
      in
      (match prefix with
      | "repo" -> (
          match repo with
          | Some repo -> repo_scope_matches ~repo target
          | None -> false)
      | "project" -> (
          match project with
          | Some project -> repo_scope_matches ~repo:project target
          | None -> false)
      | "task" -> false
      | _ -> false)

(* ------------------------------------------------------------------ *)
(* Capability mapping                                                  *)
(* ------------------------------------------------------------------ *)

let support_of ~adapter ~warn (rule : rule) =
  match rule.enforcement with
  | None -> (`Supported, [])
  | Some enforcement ->
      let cls = class_name enforcement.cls in
      let provided =
        List.assoc_opt cls adapter.controls |> Option.value ~default:[]
      in
      (* A native permission prompt is a valid consent control even when the
         adapter has no script hook. Only claim the hook gap when the adapter
         supplies no consent control at all. *)
      let empty_control_gap =
        (enforcement.cls = Consent || enforcement.cls = Prohibition)
        && enforcement.controls = []
      in
      let consent_without_hooks =
        enforcement.cls = Consent && not adapter.hooks
        && not (List.mem "permissions" provided)
      in
      let unsupported =
        if empty_control_gap then [ "(no-control)" ]
        else if consent_without_hooks then enforcement.controls
        else
          List.filter (fun control -> not (List.mem control provided))
            enforcement.controls
      in
      let support =
        if empty_control_gap || consent_without_hooks then `Unsupported
        else if unsupported = [] then `Supported
        else if List.length unsupported = List.length enforcement.controls then
          `Unsupported
        else `Partial
      in
      if empty_control_gap then
        warn
          (Printf.sprintf
             "rule '%s' declares a %s with no control; it remains unsupported"
             rule.id
             (if enforcement.cls = Consent then "consent" else "prohibition"))
      else if consent_without_hooks then
        warn
          (Printf.sprintf
             "rule '%s' requires consent controls but adapter '%s' has no host \
              hooks; it remains unsupported"
             rule.id adapter.id);
      List.iter
        (fun control ->
          warn
            (Printf.sprintf
               "rule '%s' control '%s' is unsupported by adapter '%s'; it \
                remains advisory"
               rule.id control adapter.id))
        unsupported;
      (support, unsupported)

(* ------------------------------------------------------------------ *)
(* Resolution                                                          *)
(* ------------------------------------------------------------------ *)

let resolve ?repo ?role ?client ?project ?ignore_lock ?today source =
  let today = today_value ?today () in
  let diagnostics = ref [] in
  let warnings = ref [] in
  let add diagnostic = diagnostics := diagnostic :: !diagnostics in
  let warn message = warnings := message :: !warnings in
  let ignore_lock = Option.value ignore_lock ~default:false in

  match normalize_project (Option.value project ~default:"") with
  | Error diagnostic -> Error [ diagnostic ]
  | Ok project when project <> None && repo = None ->
      Error
        [
          diag "compose.project.repo" (Option.get project) None
            "--project requires --repo so the project can be resolved inside \
             a repository context";
        ]
  | Ok project -> (
  match
    if ignore_lock then Ok None
    else
      match repo with
      | None -> Ok None
      | Some repo -> Lock.find_in_repo ~repo
  with
  | Error lock_diagnostics -> Error lock_diagnostics
  | Ok lock ->
      let requested_role = role in
      let effective_role_name =
        match requested_role with
        | Some name -> Some name
        | None -> Option.bind lock (fun lock -> lock.Lock.role)
      in
      let effective_client =
        match client with
        | Some name -> name
        | None -> (
            match Option.bind lock (fun lock -> lock.Lock.client) with
            | Some name -> name
            | None -> "generic-agents")
      in
      let selected_role =
        match effective_role_name with
        | None -> None
        | Some name -> (
            match
              List.find_opt (fun (role : role) -> role.name = name) source.roles
            with
            | Some role -> Some role
            | None ->
                add
                  (diag "compose.role.unknown" "roles" None
                     (Printf.sprintf
                        "role '%s' is not in this policy source; available roles: \
                         %s"
                        name
                        (match List.map (fun (role : role) -> role.name) source.roles with
                        | [] -> "(none)"
                        | names -> String.concat ", " names)));
                None)
      in
      let selected_adapter =
        match
          List.find_opt
            (fun (adapter : adapter) -> adapter.id = effective_client)
            source.adapters
        with
        | Some adapter -> Some adapter
        | None ->
            add
              (diag "compose.client.unknown" "adapters" None
                 (Printf.sprintf
                    "client '%s' has no adapter in this policy source; available \
                     adapters: %s"
                    effective_client
                    (match
                       List.map (fun (adapter : adapter) -> adapter.id)
                         source.adapters
                     with
                    | [] -> "(none)"
                    | ids -> String.concat ", " ids)));
            None
      in
      match selected_adapter with
      | None -> Error (normalize_diagnostics !diagnostics)
      | Some adapter -> (
          let fragments =
            select_fragments ~repo ~project ~lock ~add ~warn
          in
          let org_rules =
            source.rules
            |> List.filter (fun (rule : rule) -> rule.layer = Organization)
            |> List.sort compare_rule_by_id
          in
          let org_by_id = Hashtbl.create 16 in
          List.iter
            (fun (rule : rule) ->
              if not (Hashtbl.mem org_by_id rule.id) then
                Hashtbl.add org_by_id rule.id rule)
            org_rules;
          (* Repository fragments carry rules with the same schema as the
             organization corpus. Re-run the validator over the combined rule
             set so a fragment cannot smuggle in a placeholder owner, unknown
             action, empty prohibition, or credential-shaped body. The prose
             is scanned separately because it is rendered verbatim. *)
          (match fragments with
          | [] -> ()
          | fragments ->
              let fragment_rules =
                List.concat_map
                  (fun (fragment : fragment) -> fragment.rules)
                  fragments
              in
              let synthetic =
                { source with rules = source.rules @ fragment_rules }
              in
              (match Source.validate ~today synthetic with
              | Ok () -> ()
              | Error diagnostics ->
                  (* Cross-layer duplicate IDs are Compose's decision because a
                     fragment may legitimately supersede an organization
                     revision; keep every other schema/semantic finding. *)
                  let fragment_paths =
                    List.map (fun (rule : rule) -> rule.source) fragment_rules
                  in
                  List.iter add
                    (List.filter
                       (fun (diagnostic : Model.diagnostic) ->
                         not
                           (diagnostic.code = "rule.duplicate-id"
                           && List.mem diagnostic.path fragment_paths))
                       diagnostics));
              List.iter
                (fun fragment ->
                  if Source.has_private_absolute_path fragment.prose then
                    add
                      (diag "fragment.private-path" fragment.path None
                         "repository fragment prose contains a private absolute \
                          path");
                  List.iter
                    (fun finding ->
                      add
                        (diag "fragment.secret" fragment.path None
                           (Printf.sprintf
                              "repository fragment prose contains %s" finding)))
                    (Source.secret_findings fragment.prose))
                fragments);
          let fragment_rules =
            List.concat_map
              (fun (fragment : fragment) ->
                List.sort compare_rule_by_id fragment.rules)
              fragments
          in
          let fragment_ids = Hashtbl.create 8 in
          let removed = Hashtbl.create 8 in
          let included_fragments = ref [] in
          List.iter
            (fun (rule : rule) ->
              let path = rule.source and line = Some rule.line in
              if Hashtbl.mem fragment_ids rule.id then
                add
                  (diag "compose.rule.duplicate-id" path line
                     (Printf.sprintf
                        "repository fragment declares duplicate rule id '%s'"
                        rule.id))
              else Hashtbl.add fragment_ids rule.id ();
              let supersedes = Option.bind rule.supersedes parse_supersedes in
              let superseding_same_revision =
                match (supersedes, Hashtbl.find_opt org_by_id rule.id) with
                | Some (id, revision), Some org ->
                    id = rule.id && revision = org.revision
                    && rule.revision > revision
                | _ -> false
              in
              (match Hashtbl.find_opt org_by_id rule.id with
              | Some _ when not superseding_same_revision ->
                  add
                    (diag "compose.rule.duplicate-id" path line
                       (Printf.sprintf
                          "repository rule '%s' reuses an organization rule id \
                           without a superseding revision"
                          rule.id))
              | _ -> ());
              let ignored = ref false in
              (match rule.supersedes with
              | None -> ()
              | Some text -> (
                  match supersedes with
                  | None ->
                      add
                        (diag "compose.rule.supersedes" path line
                           (Printf.sprintf
                              "rule '%s' supersedes value '%s' must be \
                               'id@revision'"
                              rule.id text))
                  | Some (target_id, revision) -> (
                      match Hashtbl.find_opt org_by_id target_id with
                      | None ->
                          add
                            (diag "compose.rule.supersedes" path line
                               (Printf.sprintf
                                  "rule '%s' supersedes unknown organization \
                                   rule '%s'"
                                  rule.id target_id))
                      | Some target ->
                          if target_id = rule.id then
                            if revision <> target.revision
                               || rule.revision <= revision
                            then
                              add
                                (diag "compose.rule.supersedes" path line
                                   (Printf.sprintf
                                      "rule '%s' revision %d must supersede \
                                       '%s@%d' with a higher revision"
                                      rule.id rule.revision target_id
                                      target.revision))
                            else if target.final then begin
                              ignored := true;
                              warn
                                (Printf.sprintf
                                   "repository rule '%s' supersedes final \
                                    organization rule '%s@%d'; the final \
                                    organization rule stays effective"
                                   rule.id target_id target.revision)
                            end
                            else begin
                              Hashtbl.replace removed target_id ();
                              warn
                                (Printf.sprintf
                                   "repository rule '%s' supersedes \
                                    organization rule '%s@%d'"
                                   rule.id target_id target.revision)
                            end
                          else if target.final then
                            add
                              (diag "compose.override.final" path line
                                 (Printf.sprintf
                                    "repository rule '%s' supersedes final \
                                     organization rule '%s' (unauthorized \
                                     override)"
                                    rule.id target_id))
                          else begin
                            Hashtbl.replace removed target_id ();
                            warn
                              (Printf.sprintf
                                 "repository rule '%s' supersedes organization \
                                  rule '%s@%d'"
                                 rule.id target_id target.revision)
                          end)));
              List.iter
                (fun target_id ->
                  match Hashtbl.find_opt org_by_id target_id with
                  | None ->
                      add
                        (diag "compose.override.unknown" path line
                           (Printf.sprintf
                              "repository rule '%s' relaxes unknown \
                               organization rule '%s'"
                              rule.id target_id))
                  | Some target when target.final ->
                      add
                        (diag "compose.override.final" path line
                           (Printf.sprintf
                              "repository rule '%s' relaxes final organization \
                               rule '%s' (unauthorized override)"
                              rule.id target_id))
                  | Some _ ->
                      Hashtbl.replace removed target_id ();
                      warn
                        (Printf.sprintf
                           "repository rule '%s' relaxes organization rule '%s'"
                           rule.id target_id))
                rule.relaxes;
              (match (rule.key, rule.value) with
              | Some _, Some _ | None, None -> ()
              | _ ->
                  add
                    (diag "compose.rule.key-value" path line
                       (Printf.sprintf
                          "rule '%s' must provide both 'key' and 'value', or \
                           neither"
                          rule.id)));
              if not !ignored then
                included_fragments := rule :: !included_fragments)
            fragment_rules;
          (match selected_role with
          | None -> ()
          | Some role ->
              List.iter
                (fun target_id ->
                  match Hashtbl.find_opt org_by_id target_id with
                  | None ->
                      add
                        (diag "compose.override.unknown" role.source None
                           (Printf.sprintf
                              "role '%s' relaxes unknown organization rule '%s'"
                              role.name target_id))
                  | Some target when target.final ->
                      add
                        (diag "compose.override.final" role.source None
                           (Printf.sprintf
                              "role '%s' relaxes final organization rule '%s' \
                               (unauthorized override)"
                              role.name target_id))
                  | Some _ ->
                      Hashtbl.replace removed target_id ();
                      warn
                        (Printf.sprintf
                           "role '%s' relaxes organization rule '%s'" role.name
                           target_id))
                role.relaxes);
          let effective_org =
            List.filter
              (fun (rule : rule) -> not (Hashtbl.mem removed rule.id))
              org_rules
          in
          let effective_fragments =
            List.rev !included_fragments |> List.sort compare_rule_by_id
          in
          let seen_keys = Hashtbl.create 8 in
          List.iter
            (fun (rule : rule) ->
              match rule.key with
              | None -> ()
              | Some key -> (
                  match Hashtbl.find_opt seen_keys key with
                  | Some (other_id, other_value) when other_value <> rule.value
                    ->
                      add
                        (diag "compose.rule.key-conflict" rule.source
                           (Some rule.line)
                           (Printf.sprintf
                              "repository rule '%s' claims key '%s' with a \
                               different value than repository rule '%s'"
                              rule.id key other_id))
                  | Some _ -> ()
                  | None -> Hashtbl.add seen_keys key (rule.id, rule.value)))
            effective_fragments;
          let final_keys = Hashtbl.create 8 in
          List.iter
            (fun (rule : rule) ->
              if rule.final then
                match (rule.key, rule.value) with
                | Some key, Some value ->
                    Hashtbl.replace final_keys key (rule.id, value)
                | _ -> ())
            effective_org;
          List.iter
            (fun (rule : rule) ->
              match (rule.key, rule.value) with
              | Some key, Some value -> (
                  match Hashtbl.find_opt final_keys key with
                  | Some (final_id, final_value) when final_value <> value ->
                      add
                        (diag "compose.rule.final-key" rule.source
                           (Some rule.line)
                           (Printf.sprintf
                              "repository rule '%s' sets key '%s' to '%s', \
                               which conflicts with final organization rule '%s'"
                              rule.id key value final_id))
                  | _ -> ())
              | _ -> ())
            effective_fragments;
          let role_additions =
            match selected_role with
            | None -> []
            | Some role ->
                role.additions
                |> List.sort_uniq String.compare
                |> List.filter_map (fun id ->
                       if
                         Hashtbl.mem removed id
                         || List.exists
                              (fun (rule : rule) -> rule.id = id)
                              (effective_org @ effective_fragments)
                       then None
                       else
                         match Hashtbl.find_opt org_by_id id with
                         | Some rule -> Some (rule, Role)
                         | None -> None)
          in
          let effective =
            List.map (fun rule -> (rule, Organization)) effective_org
            @ List.map (fun rule -> (rule, Repository)) effective_fragments
            @ role_additions
          in
          let effective =
            List.sort
              (fun (left, left_origin) (right, right_origin) ->
                let by_origin =
                  Int.compare (origin_rank left_origin)
                    (origin_rank right_origin)
                in
                if by_origin <> 0 then by_origin
                else compare_rule_by_id left right)
              effective
          in
          let effective_ids =
            List.map (fun ((rule : rule), _) -> rule.id) effective
            |> List.sort_uniq String.compare
          in
          let active_exceptions =
            source.exceptions
            |> List.filter (fun (exception_ : exception_) ->
                   List.mem exception_.rule effective_ids
                   && exception_is_active ~repo ~project ~today exception_)
            |> List.sort compare_exception
          in
          let active_exception_ids =
            List.map (fun (exception_ : exception_) -> exception_.id)
              active_exceptions
          in
          let role_name =
            Option.map (fun (role : role) -> role.name) selected_role
          in
          let release_fragments =
            List.map
              (fun (fragment : fragment) -> (fragment.path, fragment.digest))
              fragments
          in
          let release =
            Release.build ~client:adapter.id ?role:role_name ?project
              ~fragments:release_fragments ~exceptions:active_exception_ids
              source
          in
          (match lock with
          | None -> ()
          | Some lock ->
              let re_pin = "re-pin with an explicit reviewed change" in
              let release_matches = lock.Lock.release = source.policy.release in
              let compiler_matches =
                lock.Lock.compiler = source.policy.compiler
              in
              let source_matches =
                match lock.Lock.source_digest with
                | None -> true
                | Some pinned -> pinned = Release.source_digest source
              in
              if not release_matches then
                add
                  (diag "compose.lock.release" lock.Lock.source None
                     (Printf.sprintf
                        "lock release '%s' does not match source release '%s'; \
                         %s"
                        lock.release source.policy.release re_pin));
              if not compiler_matches then
                add
                  (diag "compose.lock.release" lock.Lock.source None
                     (Printf.sprintf
                        "lock compiler '%s' does not match source compiler \
                         '%s'; %s"
                        lock.compiler source.policy.compiler re_pin));
              if not source_matches then
                add
                  (diag "compose.lock.source-digest" lock.Lock.source None
                     (Printf.sprintf
                        "pinned source digest '%s' does not match the loaded \
                         source '%s'; %s"
                        (Option.value lock.source_digest ~default:"(none)")
                        (Release.source_digest source) re_pin));
              if release_matches && compiler_matches && source_matches then
                match lock.Lock.digest with
                | None -> ()
                | Some pinned ->
                    let actual = Release.digest release in
                    if pinned <> actual then
                      add
                        (diag "compose.lock.digest" lock.Lock.source None
                           (Printf.sprintf
                              "pinned digest '%s' does not match the resolved \
                               selection '%s'; %s"
                              pinned actual re_pin)));
          if !diagnostics <> [] then
            Error (normalize_diagnostics !diagnostics)
          else
            let resolved =
              List.map
                (fun ((rule : rule), origin) ->
                  let active_exception =
                    List.find_opt
                      (fun (exception_ : exception_) ->
                        exception_.rule = rule.id)
                      active_exceptions
                  in
                  let support, unsupported_controls =
                    support_of ~adapter ~warn rule
                  in
                  {
                    rule;
                    origin;
                    active_exception;
                    support;
                    unsupported_controls;
                  })
                effective
            in
            let skills =
              List.sort
                (fun (left : skill) (right : skill) ->
                  let by_name = String.compare left.name right.name in
                  if by_name <> 0 then by_name
                  else String.compare left.source right.source)
                source.skills
            in
            Ok
              {
                source;
                release;
                digest = Release.digest release;
                source_digest = Release.source_digest source;
                adapter;
                role = selected_role;
                project;
                fragments;
                lock;
                rules = resolved;
                skills;
                exceptions = active_exceptions;
                warnings = List.sort_uniq String.compare !warnings;
                today;
              }))

let resolve_root ?repo ?role ?client ?project ?today ~root () =
  let today = today_value ?today () in
  match Source.load ~root with
  | Error diagnostics -> Error diagnostics
  | Ok source -> (
      match Source.validate ~today source with
      | Error diagnostics -> Error diagnostics
      | Ok () -> resolve ?repo ?role ?client ?project ~today source)

(* ------------------------------------------------------------------ *)
(* Explain                                                             *)
(* ------------------------------------------------------------------ *)

let strings values = `List (List.map (fun value -> `String value) values)
let option_string = function None -> `Null | Some value -> `String value

let enforcement_json (enforcement : enforcement) =
  `Assoc
    [
      ("class", `String (class_name enforcement.cls));
      ("controls", strings enforcement.controls);
    ]

let guard_json (guard : guard) =
  `Assoc
    [
      ("action", `String guard.action);
      ("paths", strings guard.paths);
      ("hosts", strings guard.hosts);
    ]

let file_list_json files =
  `List
    (List.map
       (fun (path, digest) ->
         `Assoc [ ("path", `String path); ("sha256", `String digest) ])
       files)

let rule_json (resolved : resolved_rule) =
  let rule = resolved.rule in
  `Assoc
    [
      ("id", `String rule.id);
      ("revision", `Int rule.revision);
      ("strength", `String (strength_name rule.strength));
      ("final", `Bool rule.final);
      ("kind", `String (kind_name rule.kind));
      ("layer", `String (layer_name rule.layer));
      ("origin", `String (origin_name resolved.origin));
      ("source", `String rule.source);
      ("line", `Int rule.line);
      ( "enforcement",
        match rule.enforcement with
        | None -> `Null
        | Some enforcement -> enforcement_json enforcement );
      ("actions", strings rule.actions);
      ("guards", `List (List.map guard_json rule.guards));
      ( "exception",
        match resolved.active_exception with
        | None -> `Null
        | Some exception_ -> `String exception_.id );
      ("support", `String (support_name resolved.support));
      ("unsupported_controls", strings resolved.unsupported_controls);
    ]

let skill_json (skill : skill) =
  `Assoc
    [
      ("name", `String skill.name);
      ("files", file_list_json skill.files);
      ("triggers", strings skill.triggers);
      ("rules", strings skill.rules);
    ]

let capabilities_json (adapter : adapter) =
  let controls =
    adapter.controls
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.map (fun (cls, controls) -> (cls, strings controls))
  in
  `Assoc
    [
      ("adapter", `Assoc [ ("id", `String adapter.id); ("version", `Int adapter.version) ]);
      ( "instructions",
        `Assoc
          [
            ("file", `String adapter.instructions.file);
            ("discovery", strings adapter.instructions.discovery);
            ("includes", `Bool adapter.instructions.includes);
          ] );
      ( "skills",
        `Assoc
          [
            ("dir", `String adapter.skills.dir);
            ("auto_discovery", `Bool adapter.skills.auto_discovery);
            ("explicit_reference", `Bool adapter.skills.explicit_reference);
          ] );
      ("roles", `Bool adapter.roles);
      ("mcp", `Bool adapter.mcp);
      ("hooks", `Bool adapter.hooks);
      ("permissions", `Bool adapter.permissions);
      ("atomic_output", `Bool adapter.atomic_output);
      ("controls", `Assoc controls);
      ("unsupported", strings adapter.unsupported);
    ]

type override = {
  kind : string;
  rule_id : string;
  target : string;
  source : string;
  line : int option;
}

let overrides (policy_set : t) =
  let applied_repository_ids =
    policy_set.rules
    |> List.filter_map (fun (resolved : resolved_rule) ->
           match resolved.origin with
           | Repository -> Some resolved.rule.id
           | Organization | Role -> None)
    |> List.sort_uniq String.compare
  in
  let from_fragment =
    policy_set.fragments
    |> List.concat_map (fun (fragment : fragment) ->
           List.filter
             (fun (rule : rule) -> List.mem rule.id applied_repository_ids)
             fragment.rules
           |> List.concat_map (fun (rule : rule) ->
                  List.map
                    (fun target ->
                      {
                        kind = "relax";
                        rule_id = rule.id;
                        target;
                        source = rule.source;
                        line = Some rule.line;
                      })
                    rule.relaxes
                  @
                  match rule.supersedes with
                  | None -> []
                  | Some target ->
                      [
                        {
                          kind = "supersede";
                          rule_id = rule.id;
                          target;
                          source = rule.source;
                          line = Some rule.line;
                        };
                      ]))
  in
  let from_role =
    match policy_set.role with
    | None -> []
    | Some role ->
        List.map
          (fun target ->
            {
              kind = "role-relax";
              rule_id = role.name;
              target;
              source = role.source;
              line = None;
            })
          role.relaxes
  in
  let compare_override left right =
    let by_kind = String.compare left.kind right.kind in
    if by_kind <> 0 then by_kind
    else
      let by_rule = String.compare left.rule_id right.rule_id in
      if by_rule <> 0 then by_rule
      else
        let by_target = String.compare left.target right.target in
        if by_target <> 0 then by_target
        else String.compare left.source right.source
  in
  List.sort compare_override (from_fragment @ from_role)

let override_json (override : override) =
  `Assoc
    [
      ("kind", `String override.kind);
      ("rule", `String override.rule_id);
      ("target", `String override.target);
      ("source", `String override.source);
      ( "line",
        match override.line with None -> `Null | Some line -> `Int line );
    ]

let lock_json (lock : Lock.t) =
  `Assoc
    [
      ("schema", `String lock.Lock.schema);
      ("release", `String lock.Lock.release);
      ("digest", option_string lock.Lock.digest);
      ("source_digest", option_string lock.Lock.source_digest);
      ("adapter_schema", `Int lock.Lock.adapter_schema);
      ("compiler", `String lock.Lock.compiler);
      ("role", option_string lock.Lock.role);
      ("client", option_string lock.Lock.client);
      ("fragment", option_string lock.Lock.fragment);
    ]

let explain_json (policy_set : t) =
  `Assoc
    [
      ("schema", `String "nomia-explain/v2");
      ( "release",
        `Assoc
          [
            ("version", `String policy_set.source.policy.release);
            ("compiler", `String policy_set.source.policy.compiler);
            ("digest", `String policy_set.digest);
          ] );
      ("source_digest", `String policy_set.source_digest);
      ("project", option_string policy_set.project);
      ( "fragments",
        `List
          (List.map
             (fun (fragment : fragment) ->
               `Assoc
                 [
                   ( "scope",
                     `String
                       (match fragment.scope with
                       | Root -> "root"
                       | Project -> "project") );
                   ("path", `String fragment.path);
                   ("digest", `String fragment.digest);
                 ])
             policy_set.fragments) );
      ( "role",
        option_string
          (Option.map (fun (role : role) -> role.name) policy_set.role) );
      ( "role_additions",
        strings
          (match policy_set.role with
          | None -> []
          | Some role -> List.sort_uniq String.compare role.additions) );
      ( "adapter",
        `Assoc
          [
            ("id", `String policy_set.adapter.id);
            ("version", `Int policy_set.adapter.version);
          ] );
      ( "exceptions",
        strings
          (List.map
             (fun (exception_ : exception_) -> exception_.id)
             policy_set.exceptions) );
      ( "lock",
        match policy_set.lock with None -> `Null | Some lock -> lock_json lock );
      ("rules", `List (List.map rule_json policy_set.rules));
      ("skills", `List (List.map skill_json policy_set.skills));
      ("capabilities", capabilities_json policy_set.adapter);
      ("overrides", `List (List.map override_json (overrides policy_set)));
      ("warnings", strings policy_set.warnings);
    ]

let explain_text (policy_set : t) =
  let buffer = Buffer.create 2048 in
  let line format =
    Printf.ksprintf
      (fun text ->
        Buffer.add_string buffer text;
        Buffer.add_char buffer '\n')
      format
  in
  line "nomia explain";
  line "release: %s" policy_set.source.policy.release;
  line "compiler: %s" policy_set.source.policy.compiler;
  line "digest: %s" policy_set.digest;
  line "source_digest: %s" policy_set.source_digest;
  line "adapter: %s@%d" policy_set.adapter.id policy_set.adapter.version;
  line "project: %s"
    (Option.value policy_set.project ~default:"none");
  line "role: %s"
    (match policy_set.role with None -> "none" | Some role -> role.name);
  line "role_additions: %s"
    (match policy_set.role with
    | None -> "none"
    | Some role -> (
        match List.sort_uniq String.compare role.additions with
        | [] -> "none"
        | additions -> String.concat ", " additions));
  line "fragments: %s"
    (match policy_set.fragments with
    | [] -> "none"
    | fragments ->
        String.concat ", "
          (List.map
             (fun (fragment : fragment) ->
               Printf.sprintf "%s=%s"
                 (match fragment.scope with
                 | Root -> "root"
                 | Project -> "project")
                 fragment.path)
             fragments));
  line "lock: %s"
    (match policy_set.lock with
    | None -> "none"
    | Some lock -> Printf.sprintf "%s (%s)" lock.Lock.release lock.Lock.source);
  line "exceptions: %s"
    (match policy_set.exceptions with
    | [] -> "none"
    | exceptions ->
        String.concat ", "
          (List.map (fun (exception_ : exception_) -> exception_.id) exceptions));
  line "rules:";
  List.iter
    (fun (resolved : resolved_rule) ->
      line "  %s rev %d [%s/%s%s] %s" resolved.rule.id resolved.rule.revision
        (origin_name resolved.origin)
        (support_name resolved.support)
        (if resolved.rule.final then " final" else "")
        (match resolved.active_exception with
        | None -> ""
        | Some exception_ -> Printf.sprintf " exception=%s" exception_.id))
    policy_set.rules;
  line "skills: %s"
    (match policy_set.skills with
    | [] -> "none"
    | skills -> String.concat ", " (List.map (fun (skill : skill) -> skill.name) skills));
  line "warnings:";
  (match policy_set.warnings with
  | [] -> line "  none"
  | warnings -> List.iter (fun warning -> line "  %s" warning) warnings);
  Buffer.contents buffer
