(** Atomic synchronization and drift diagnosis for managed targets. *)

type error = Diagnostics of Model.diagnostic list | Message of string

type sync_result = {
  state : Store.state;
  changed : bool;
  lock_updated : bool;
  warnings : string list;
}

(* ------------------------------------------------------------------ *)
(* Small helpers                                                       *)
(* ------------------------------------------------------------------ *)

let starts_with ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.sub value 0 prefix_len = prefix

let contains_sub text needle =
  let text_len = String.length text and needle_len = String.length needle in
  if needle_len = 0 then true
  else
    let rec loop index =
      if index + needle_len > text_len then false
      else if String.sub text index needle_len = needle then true
      else loop (index + 1)
    in
    loop 0

let lstat_opt path =
  try Some (Unix.lstat path)
  with Unix.Unix_error (Unix.ENOENT, _, _) | Sys_error _ -> None

let is_directory path =
  match lstat_opt path with
  | Some stats -> stats.Unix.st_kind = Unix.S_DIR
  | None -> false

let is_symlink path =
  match lstat_opt path with
  | Some stats -> stats.Unix.st_kind = Unix.S_LNK
  | None -> false

let sort_uniq values = List.sort_uniq String.compare values

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let jstring = function `String value -> value | _ -> ""
let jlist = function `List values -> values | _ -> []

let string_list json key =
  match member key json with
  | Some (`List values) ->
      List.filter_map (function `String value -> Some value | _ -> None) values
  | _ -> []

let diagnostic code path message : Model.diagnostic =
  { code; path; line = None; message }

let load_source root =
  Source.load_and_validate ~root ?today:None

(* ------------------------------------------------------------------ *)
(* Release construction and candidate validation                       *)
(* ------------------------------------------------------------------ *)

let release_for_compose (policy_set : Compose.t) artifacts =
  let role =
    Option.map (fun (role : Model.role) -> role.name) policy_set.Compose.role
  in
  let fragments =
    List.map
      (fun (fragment : Compose.fragment) ->
        (fragment.path, fragment.digest))
      policy_set.Compose.fragments
  in
  let exceptions =
    List.map
      (fun (exception_ : Model.exception_) -> exception_.id)
      policy_set.Compose.exceptions
  in
  Release.build ~client:policy_set.Compose.adapter.Model.id ?role
    ?project:policy_set.Compose.project
    ~inherit_root:policy_set.Compose.inherit_root ~fragments ~exceptions
    ~artifacts policy_set.Compose.source

let artifact_triples (artifacts : Render.artifact list) =
  List.map
    (fun (artifact : Render.artifact) ->
      (artifact.path, artifact.contents, artifact.mode))
    artifacts

let verify_candidate_manifest manifest triples =
  let json = Release.manifest_json manifest in
  match member "artifacts" json with
  | Some (`List entries) ->
      let rec loop = function
        | [] -> Ok ()
        | `Assoc _ as entry :: rest -> (
            match (member "path" entry, member "sha256" entry) with
            | Some (`String path), Some (`String expected) -> (
                match
                  List.find_opt (fun (candidate, _, _) -> candidate = path) triples
                with
                | None ->
                    Error (Message ("rendered artifact is missing: " ^ path))
                | Some (_, contents, _) ->
                    let actual = Canonical.sha256_hex contents in
                    if actual = expected then loop rest
                    else
                      Error
                        (Message
                           (Printf.sprintf
                              "rendered artifact '%s' does not match its \
                               manifest hash"
                              path)))
            | _ ->
                Error
                  (Message
                     "release manifest artifact entries need path and sha256"))
        | _ :: _ ->
            Error (Message "release manifest artifacts must be objects")
      in
      loop entries
  | _ -> Error (Message "release manifest has no artifacts list")

let entrypoints_for (policy_set : Compose.t) (artifacts : Render.artifact list) =
  let adapter = policy_set.Compose.adapter in
  let present path =
    List.exists (fun (artifact : Render.artifact) -> artifact.path = path) artifacts
  in
  let files =
    [ "AGENTS.md"; adapter.Model.instructions.file; "opencode.json";
      "framework.md"; "persona.md" ]
    |> List.filter present
  in
  let skills =
    let directory = adapter.Model.skills.dir in
    if
      directory <> ""
      && List.exists
           (fun (artifact : Render.artifact) ->
             starts_with ~prefix:(directory ^ "/") artifact.path)
           artifacts
    then [ directory ]
    else []
  in
  sort_uniq (files @ skills)

let entrypoint_present entrypoints triples =
  List.for_all
    (fun entrypoint ->
      List.exists
        (fun (path, _, _) ->
          path = entrypoint
          || starts_with ~prefix:(entrypoint ^ "/") path)
        triples)
    entrypoints

(* ------------------------------------------------------------------ *)
(* Preflight: refuse unmanaged or hand-edited targets untouched        *)
(* ------------------------------------------------------------------ *)

let conflict message = Error (Message message)

let check_entrypoint target entrypoint =
  let path = Filename.concat target.Store.root entrypoint in
  match lstat_opt path with
  | None -> Ok ()
  | Some stats when stats.Unix.st_kind = Unix.S_LNK -> (
      match Store.read_symlink path with
      | Error message -> conflict message
      | Ok None -> conflict (Printf.sprintf "cannot read entrypoint '%s'" entrypoint)
      | Ok (Some link) ->
          if Filename.is_relative link
             && contains_sub link (Store.managed_dir ^ "/current/")
          then Ok ()
          else
            conflict
              (Printf.sprintf
                 "entrypoint '%s' is a symlink that sync did not create; \
                  refusing to overwrite it"
                 entrypoint))
  | Some _ ->
      if entrypoint = "AGENTS.md" then
        conflict
          "hand-authored AGENTS.md is a conflict: move the repository prose to \
           AGENTS.fragment.md and remove AGENTS.md before syncing; sync never \
           overwrites it"
      else
        conflict
          (Printf.sprintf
             "entrypoint '%s' already exists and is not a managed symlink; \
              move or remove it before syncing"
             entrypoint)

let check_parents target entrypoint =
  let parts = String.split_on_char '/' entrypoint in
  let rec loop relative = function
    | [] | [ _ ] -> Ok ()
    | part :: rest ->
        let relative = if relative = "" then part else relative ^ "/" ^ part in
        let path = Filename.concat target.Store.root relative in
        (match lstat_opt path with
        | None -> Ok ()
        | Some stats when stats.Unix.st_kind = Unix.S_DIR -> loop relative rest
        | Some _ ->
            conflict
              (Printf.sprintf
                 "entrypoint '%s' needs '%s' to be a directory, but a file is \
                  there; move it before syncing"
                 entrypoint relative))
  in
  loop "" parts

let check_existing_managed target ~client =
  match lstat_opt target.Store.managed with
  | None -> Ok ()
  | Some stats when stats.Unix.st_kind <> Unix.S_DIR ->
      conflict
        (Printf.sprintf
           "'%s' exists but is not a directory; refusing to overwrite it"
           Store.managed_dir)
  | Some _ ->
      let state_exists = Store.file_exists target.Store.state_path in
      let current_exists = lstat_opt target.Store.current <> None in
      if state_exists then begin
        match Store.effective_state target with
        | Error message ->
            conflict ("managed state.json is unreadable: " ^ message)
        | Ok None -> conflict "managed state.json disappeared while reading it"
        | Ok (Some state) -> (
            if state.Store.client <> client then
              conflict
                (Printf.sprintf
                   "target is managed for client '%s'; sync does not switch \
                    clients in place. Remove .nomia or use a fresh \
                    target."
                   state.Store.client)
            else if not current_exists then
              conflict "managed state.json exists but current is missing"
            else
              match Store.read_symlink target.Store.current with
              | Error message -> conflict message
              | Ok None ->
                  conflict "managed current is not a symbolic link"
              | Ok (Some link) ->
                  let expected = "releases/" ^ state.Store.digest in
                  if link <> expected then
                    conflict
                      "managed current does not select the digest in state.json"
                  else
                    match
                      Store.verify_release target ~digest:state.Store.digest
                    with
                    | Ok () -> Ok ()
                    | Error message ->
                        conflict
                          ("installed release is hand-edited: " ^ message))
      end
      else if current_exists then
        conflict "managed current exists without state.json"
      else
        let entries = Store.list_dir target.Store.managed in
        if entries = [] || entries = [ "releases" ] then
          if Store.file_exists target.Store.releases
             && not (is_directory target.Store.releases)
          then conflict "managed releases is not a directory"
          else Ok ()
        else
          conflict
            "managed root contains files that sync did not create; refusing to \
             overwrite them"

let preflight target ~client entrypoints =
  match lstat_opt target.Store.root with
  | Some stats when stats.Unix.st_kind <> Unix.S_DIR ->
      conflict "target exists but is not a directory"
  | _ -> (
      match check_existing_managed target ~client with
      | Error error -> Error error
      | Ok () ->
          let rec loop = function
            | [] -> Ok ()
            | entrypoint :: rest -> (
                match check_parents target entrypoint with
                | Error error -> Error error
                | Ok () -> (
                    match check_entrypoint target entrypoint with
                    | Error error -> Error error
                    | Ok () -> loop rest))
          in
          loop entrypoints)

(* ------------------------------------------------------------------ *)
(* Staging                                                            *)
(* ------------------------------------------------------------------ *)

let fault_before_swap () =
  match Sys.getenv_opt "NOMIA_FAULT" with
  | Some "before-swap" -> true
  | _ -> false

let fault_after_swap () =
  match Sys.getenv_opt "NOMIA_FAULT" with
  | Some "after-swap" -> true
  | _ -> false

let prepare_release target ~digest ~triples ~manifest =
  let directory = Store.release_dir target ~digest in
  if is_directory directory then
    match Store.verify_release target ~digest with
    | Ok () -> Ok true
    | Error message ->
        conflict
          (Printf.sprintf
             "release '%s' already exists but does not verify: %s; restore or \
              remove it before syncing"
             digest message)
  else
    match Store.mkdir_p target.Store.releases with
    | Error message -> Error (Message message)
    | Ok () ->
        let stage = Store.staging_path target in
        Store.remove_tree stage;
        let cleanup () = Store.remove_tree stage in
        let write_artifacts () =
          let rec loop = function
            | [] -> Ok ()
            | (path, contents, mode) :: rest -> (
                match
                  Store.write_file
                    ~path:(Filename.concat stage path)
                    ~mode contents
                with
                | Error message -> Error (Message message)
                | Ok () -> loop rest)
          in
          loop triples
        in
        (match Store.mkdir_p stage with
        | Error message -> Error (Message message)
        | Ok () -> (
            match write_artifacts () with
            | Error error ->
                cleanup ();
                Error error
            | Ok () -> (
                match
                  Store.write_file
                    ~path:
                      (Filename.concat stage ".nomia/manifest.json")
                    ~mode:0o644
                    (Release.to_string manifest ^ "\n")
                with
                | Error message ->
                    cleanup ();
                    Error (Message message)
                | Ok () -> (
                    match Store.verify_tree ~root:stage ~digest with
                    | Error message ->
                        cleanup ();
                        conflict
                          ("staged release does not verify: " ^ message)
                    | Ok () ->
                        if fault_before_swap () then begin
                          cleanup ();
                          Error
                            (Message
                               "injected fault before swapping current \
                                (NOMIA_FAULT=before-swap); the previous \
                                target is unchanged")
                        end
                        else
                          try
                            Unix.rename stage directory;
                            Ok false
                          with Unix.Unix_error (error, function_name, argument) ->
                            cleanup ();
                            if is_directory directory then
                              match Store.verify_release target ~digest with
                              | Ok () -> Ok true
                              | Error message ->
                                  conflict
                                    (Printf.sprintf
                                       "release '%s' appeared but does not \
                                        verify: %s"
                                       digest message)
                            else
                              Error
                                (Message
                                   (Printf.sprintf "%s: %s %s"
                                      (Unix.error_message error) function_name
                                      argument))))))

(* ------------------------------------------------------------------ *)
(* Lock pin                                                            *)
(* ------------------------------------------------------------------ *)

let root_fragment (policy_set : Compose.t) =
  policy_set.Compose.fragments
  |> List.find_map (fun (fragment : Compose.fragment) ->
         match fragment.Compose.scope with
         | Compose.Root -> Some fragment.Compose.path
         | Compose.Project -> None)

let lock_of_candidate (policy_set : Compose.t) =
  let role =
    Option.map (fun (role : Model.role) -> role.name) policy_set.Compose.role
  in
  let fragment = root_fragment policy_set in
  {
    Lock.schema = Lock.schema;
    release = policy_set.Compose.source.Model.policy.release;
    digest = None;
    source_digest = Some policy_set.Compose.source_digest;
    adapter_schema = 1;
    compiler = policy_set.Compose.source.Model.policy.compiler;
    role;
    client = Some policy_set.Compose.adapter.Model.id;
    fragment;
    source = Lock.lock_file;
  }

let lock_mismatches (lock : Lock.t) (policy_set : Compose.t) =
  let role =
    Option.map (fun (role : Model.role) -> role.name) policy_set.Compose.role
  in
  let fragment = root_fragment policy_set in
  let check label left right =
    if left = right then [] else [ label ]
  in
  check "release" (Some lock.Lock.release)
    (Some policy_set.Compose.source.Model.policy.release)
  @ check "compiler" (Some lock.Lock.compiler)
      (Some policy_set.Compose.source.Model.policy.compiler)
  @ (match lock.Lock.digest with
    | None -> []
    | Some digest -> check "digest" (Some digest) (Some policy_set.Compose.digest))
  @ check "source_digest" lock.Lock.source_digest
      (Some policy_set.Compose.source_digest)
  @
  (* Role and client in a source pin are defaults for omitted options, not
     exclusive selections: one root pin serves several project/client/role
     renderings. An explicit legacy digest pin still fixes the selection. *)
  (match lock.Lock.digest with
  | None -> []
  | Some _ ->
      check "role" lock.Lock.role role
      @ check "client" lock.Lock.client
          (Some policy_set.Compose.adapter.Model.id))
  @ check "fragment" lock.Lock.fragment fragment
  @ if lock.Lock.adapter_schema = 1 then [] else [ "adapter_schema" ]

let write_lock repo lock =
  Store.atomic_write
    ~path:(Filename.concat repo Lock.lock_file)
    ~mode:0o644 (Lock.to_yaml_string lock)

(* ------------------------------------------------------------------ *)
(* Installation                                                       *)
(* ------------------------------------------------------------------ *)

let rollback target installed =
  match installed with
  | Some old ->
      ignore (Store.write_state target old);
      ignore (Store.swap_current target ~digest:old.Store.digest)
  | None ->
      (try Sys.remove target.Store.state_path
       with Sys_error _ | Unix.Unix_error _ -> ());
      (try Unix.unlink target.Store.current
       with Sys_error _ | Unix.Unix_error _ -> ())

let install ~target_root ~repo ~source:_ ~policy_set ~update_pin =
  let client_name = policy_set.Compose.adapter.Model.id in
  match Render.render policy_set ~client:client_name with
  | Error diagnostics -> Error (Diagnostics diagnostics)
  | Ok artifacts ->
      let triples = artifact_triples artifacts in
      let artifacts_for_manifest =
        List.map (fun (path, contents, _) -> (path, contents)) triples
      in
      let manifest = release_for_compose policy_set artifacts_for_manifest in
      if Release.digest manifest <> policy_set.Compose.digest then
        Error
          (Message
             "internal release digest mismatch between composition and manifest")
      else
        match verify_candidate_manifest manifest triples with
        | Error error -> Error error
        | Ok () ->
            let entrypoints = entrypoints_for policy_set artifacts in
            if not (entrypoint_present entrypoints triples) then
              Error
                (Message
                   "rendered release does not contain every intended entrypoint")
            else
              let target = Store.target_of_root target_root in
              (* Promote a staged state left by a crash after the current swap
                 but before the state rename, so first-sync recovery does not
                 look like an unmanaged current. *)
              ignore (Store.recover_state target);
              match preflight target ~client:client_name entrypoints with
              | Error error -> Error error
              | Ok () ->
                  let warnings = ref [] in
                  (match
                     Store.cache_release ~digest:policy_set.Compose.digest
                       ~manifest:(Release.manifest_json manifest)
                       ~explain:(Compose.explain_json policy_set)
                       ~artifacts:triples
                   with
                  | Ok () -> ()
                  | Error message ->
                      warnings :=
                        ("release cache was not updated: " ^ message)
                        :: !warnings);
                  let installed =
                    match Store.effective_state target with
                    | Ok state -> state
                    | Error _ -> None
                  in
                  let state =
                    {
                      Store.schema = Store.state_schema;
                      release = policy_set.Compose.source.Model.policy.release;
                      digest = policy_set.Compose.digest;
                      source_digest = policy_set.Compose.source_digest;
                      client = policy_set.Compose.adapter.Model.id;
                      role =
                        Option.map
                          (fun (role : Model.role) -> role.name)
                          policy_set.Compose.role;
                      compiler =
                        policy_set.Compose.source.Model.policy.compiler;
                      entrypoints;
                    }
                  in
                  let same_state =
                    match installed with
                    | Some old ->
                        old.Store.digest = state.Store.digest
                        && old.source_digest = state.source_digest
                        && old.release = state.release
                        && old.client = state.client
                        && old.role = state.role
                        && old.compiler = state.compiler
                        && old.entrypoints = state.entrypoints
                    | None -> false
                  in
                  let current_same =
                    match Store.current_digest target with
                    | Ok (Some digest) -> digest = state.Store.digest
                    | _ -> false
                  in
                  let release_intact =
                    match
                      Store.verify_release target ~digest:state.Store.digest
                    with
                    | Ok () -> true
                    | Error _ -> false
                  in
                  let already_current =
                    same_state && current_same && release_intact
                  in
                  (match
                     prepare_release target ~digest:state.Store.digest ~triples
                       ~manifest
                   with
                  | Error error -> Error error
                  | Ok _release_preexisting ->
                      if already_current then (
                        match
                          Store.ensure_entrypoints target
                            ~entrypoints:state.Store.entrypoints
                        with
                        | Error message -> Error (Message message)
                        | Ok () -> (
                            match
                              (match repo with
                              | Some repo when update_pin ->
                                  write_lock repo (lock_of_candidate policy_set)
                              | _ -> Ok ())
                            with
                            | Error message -> Error (Message message)
                            | Ok () ->
                                Ok
                                  {
                                    state;
                                    changed = false;
                                    lock_updated = repo <> None && update_pin;
                                    warnings = List.rev !warnings;
                                  }))
                      else (
                        match Store.write_state_next target state with
                        | Error message -> Error (Message message)
                        | Ok () -> (
                            match
                              Store.swap_current target ~digest:state.Store.digest
                            with
                            | Error message ->
                                Store.discard_state_next target;
                                (match installed with
                                | Some old ->
                                    ignore (Store.write_state target old)
                                | None ->
                                    (try
                                       Sys.remove target.Store.state_path
                                     with Sys_error _ | Unix.Unix_error _ -> ()));
                                Error (Message message)
                            | Ok () when fault_after_swap () ->
                                Error
                                  (Message
                                     "injected fault after swapping current \
                                      (NOMIA_FAULT=after-swap); the new \
                                      release is complete and the staged state \
                                      is recoverable by the next sync/doctor")
                            | Ok () -> (
                                match Store.promote_state_next target with
                                | Error message -> Error (Message message)
                                | Ok () -> (
                                match
                                  Store.ensure_entrypoints target
                                    ~entrypoints:state.Store.entrypoints
                                with
                                | Error message ->
                                    Store.discard_state_next target;
                                    rollback target installed;
                                    Error (Message message)
                                | Ok () ->
                                    ignore
                                      (Store.prune_releases target
                                         ~current:state.Store.digest ~keep:3);
                                    (match
                                       (match repo with
                                       | Some repo when update_pin ->
                                           write_lock repo
                                             (lock_of_candidate policy_set)
                                       | _ -> Ok ())
                                     with
                                    | Error message -> Error (Message message)
                                    | Ok () ->
                                        Ok
                                          {
                                            state;
                                            changed = true;
                                            lock_updated =
                                              repo <> None && update_pin;
                                            warnings = List.rev !warnings;
                                          }))))))

(* ------------------------------------------------------------------ *)
(* Sync entry points                                                  *)
(* ------------------------------------------------------------------ *)

let sync ~repo ~client ~role ~project ~inherit_root ~source_root ~update_pin =
  if repo = None && client = None then
    Error
      (Message
         "sync needs a target: pass --repo DIR or --client CLIENT")
  else if repo = None && update_pin then
    Error (Message "--update-pin only applies to a repository target (--repo)")
  else if repo = None && project <> None then
    Error
      (Message
         "--project only applies to a repository target; user-level sync has no \
          project context")
  else
    match load_source source_root with
    | Error diagnostics -> Error (Diagnostics diagnostics)
    | Ok source -> (
        match repo with
        | None ->
            let client = Option.get client in
            (match Compose.resolve ?role ~client ?today:None source with
            | Error diagnostics -> Error (Diagnostics diagnostics)
            | Ok policy_set ->
                install ~target_root:(Store.client_target ~client) ~repo:None
                  ~source ~policy_set ~update_pin:false)
        | Some repo ->
            let lock_result = Lock.find_in_repo ~repo in
            (match lock_result with
            | Error diagnostics when not update_pin ->
                Error (Diagnostics diagnostics)
            | _ ->
                let repo_has_lock =
                  Store.file_exists (Filename.concat repo Lock.lock_file)
                in
                let lock =
                  match lock_result with
                  | Ok lock -> lock
                  | Error _ -> None
                in
                if not repo_has_lock && not update_pin then
                  Error
                    (Message
                       "repository has no nomia.lock; run sync --repo \
                        with --update-pin to create the reviewable pin")
                else
                  let client_name =
                    match client with
                    | Some client -> client
                    | None -> (
                        match lock with
                        | Some lock ->
                            Option.value lock.Lock.client
                              ~default:"generic-agents"
                        | None -> "generic-agents")
                  in
                  let role_name =
                    match role with
                    | Some role -> Some role
                    | None -> Option.bind lock (fun lock -> lock.Lock.role)
                  in
                  let root_state =
                    Store.effective_state (Store.target_of_root repo)
                  in
                  let inherit_error =
                    if not inherit_root then None
                    else
                      match project with
                      | None ->
                          Some
                            "--inherit-root requires --project inside the \
                             managed repository"
                      | Some _ -> (
                          match root_state with
                          | Ok (Some state)
                            when state.Store.client = client_name ->
                              None
                          | Ok (Some state) ->
                              Some
                                (Printf.sprintf
                                   "--inherit-root needs a repository root \
                                    target for client '%s'; the current root \
                                    target belongs to '%s'"
                                   client_name state.Store.client)
                          | _ ->
                              Some
                                "--inherit-root needs an installed repository \
                                 root target; run the root sync first")
                  in
                  (match inherit_error with
                  | Some message -> Error (Message message)
                  | None ->
                  let target_root =
                    match project with
                    | None -> repo
                    | Some project -> Filename.concat repo project
                  in
                  let resolved =
                    match
                      Compose.resolve ~repo ?project ~inherit_root
                        ?role:role_name ~client:client_name
                        ~ignore_lock:(update_pin && repo_has_lock) ?today:None
                        source
                    with
                    | Ok policy_set -> Ok policy_set
                    | Error diagnostics -> Error (Diagnostics diagnostics)
                  in
                  (match resolved with
                  | Error error -> Error error
                  | Ok policy_set -> (
                      match lock with
                      | Some lock when not update_pin -> (
                          match lock_mismatches lock policy_set with
                          | [] ->
                              install ~target_root ~repo:(Some repo) ~source
                                ~policy_set ~update_pin
                          | fields ->
                              Error
                                (Message
                                   (Printf.sprintf
                                      "nomia.lock does not match the \
                                       selected source (%s); review the change \
                                       and re-run with --update-pin to rewrite \
                                       only the lock"
                                      (String.concat ", " fields))))
                      | _ ->
                          install ~target_root ~repo:(Some repo) ~source
                            ~policy_set ~update_pin)))))

(* ------------------------------------------------------------------ *)
(* Doctor                                                             *)
(* ------------------------------------------------------------------ *)

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
  rules : int;
  supported : int;
  partial : int;
  unsupported : int;
  unsupported_rules : string list;
  unsupported_controls : string list;
}

type issue = { code : string; message : string }

type report = {
  state : doctor_state;
  target_kind : string;
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
  inherit_root : bool;
  cached_digests : string list;
  entrypoints : string list;
  coverage : coverage;
  issues : issue list;
  warning : string option;
}

let empty_coverage =
  {
    rules = 0;
    supported = 0;
    partial = 0;
    unsupported = 0;
    unsupported_rules = [];
    unsupported_controls = [];
  }

let add_unique value values =
  if List.mem value values then values else value :: values

let coverage_of_rules rules =
  let coverage =
    List.fold_left
      (fun coverage (resolved : Compose.resolved_rule) ->
        match resolved.Compose.rule.Model.enforcement with
        | None -> coverage
        | Some _ ->
            let coverage =
              { coverage with rules = coverage.rules + 1 }
            in
            match resolved.Compose.support with
            | `Supported ->
                { coverage with supported = coverage.supported + 1 }
            | `Partial ->
                {
                  coverage with
                  partial = coverage.partial + 1;
                  unsupported_rules =
                    add_unique resolved.Compose.rule.Model.id
                      coverage.unsupported_rules;
                  unsupported_controls =
                    List.fold_left
                      (fun controls control -> add_unique control controls)
                      coverage.unsupported_controls
                      resolved.Compose.unsupported_controls;
                }
            | `Unsupported ->
                {
                  coverage with
                  unsupported = coverage.unsupported + 1;
                  unsupported_rules =
                    add_unique resolved.Compose.rule.Model.id
                      coverage.unsupported_rules;
                  unsupported_controls =
                    List.fold_left
                      (fun controls control -> add_unique control controls)
                      coverage.unsupported_controls
                      resolved.Compose.unsupported_controls;
                })
      empty_coverage rules
  in
  {
    coverage with
    unsupported_rules = List.sort String.compare coverage.unsupported_rules;
    unsupported_controls =
      List.sort String.compare coverage.unsupported_controls;
  }

let coverage_of_explain json =
  let rules = jlist (Option.value (member "rules" json) ~default:(`List [])) in
  List.fold_left
    (fun coverage rule ->
      match member "enforcement" rule with
      | Some `Null | None -> coverage
      | Some _ ->
          let id = jstring (Option.value (member "id" rule) ~default:`Null) in
          let support =
            jstring (Option.value (member "support" rule) ~default:`Null)
          in
          let controls = string_list rule "unsupported_controls" in
          let coverage = { coverage with rules = coverage.rules + 1 } in
          match support with
          | "partial" ->
              {
                coverage with
                partial = coverage.partial + 1;
                unsupported_rules = add_unique id coverage.unsupported_rules;
                unsupported_controls =
                  List.fold_left
                    (fun acc control -> add_unique control acc)
                    coverage.unsupported_controls controls;
              }
          | "unsupported" ->
              {
                coverage with
                unsupported = coverage.unsupported + 1;
                unsupported_rules = add_unique id coverage.unsupported_rules;
                unsupported_controls =
                  List.fold_left
                    (fun acc control -> add_unique control acc)
                    coverage.unsupported_controls controls;
              }
          | _ -> { coverage with supported = coverage.supported + 1 })
    empty_coverage rules
  |> fun coverage ->
  {
    coverage with
    unsupported_rules = List.sort String.compare coverage.unsupported_rules;
    unsupported_controls =
      List.sort String.compare coverage.unsupported_controls;
  }

type installed = {
  state : Store.state option;
  current : string option;
  managed_exists : bool;
  issues : issue list;
  integrity : (unit, string) result;
}

let installed_info root =
  let target = Store.target_of_root root in
  let issues = ref [] in
  let add code message =
    if not (List.exists (fun issue -> issue.code = code && issue.message = message) !issues) then
      issues := { code; message } :: !issues
  in
  let state =
    match Store.effective_state target with
    | Ok state -> state
    | Error message ->
        add "doctor.state" message;
        None
  in
  let current =
    match Store.read_symlink target.Store.current with
    | Error message ->
        add "doctor.current" message;
        None
    | Ok None -> None
    | Ok (Some link) ->
        let prefix = "releases/" in
        if starts_with ~prefix link then
          Some
            (String.sub link (String.length prefix)
               (String.length link - String.length prefix))
        else begin
          add "doctor.current"
            "managed current does not name a release under .nomia/releases";
          None
        end
  in
  let releases =
    Store.list_dir target.Store.releases
    |> List.filter (fun name -> name <> "" && name.[0] <> '.')
    |> List.filter (fun name ->
           is_directory (Filename.concat target.Store.releases name))
  in
  let managed_exists = state <> None || current <> None || releases <> [] in
  let pending_state =
    match (Store.read_state_next target, current) with
    | Ok (Some staged), Some digest ->
        staged.Store.digest = digest
        &&
        (match Store.read_state target with
        | Ok (Some durable) -> durable.Store.digest <> digest
        | Ok None -> true
        | Error _ -> true)
    | _ -> false
  in
  (match state with
  | None ->
      if managed_exists then
        add "doctor.state"
          "managed layout exists without a readable state.json"
  | Some state ->
      let expected_entrypoints = sort_uniq state.Store.entrypoints in
      if expected_entrypoints <> state.Store.entrypoints then
        add "doctor.state" "state entrypoints are not sorted and unique";
      if current <> Some state.Store.digest then
        add "doctor.current"
          "managed current does not select the digest in state.json";
      if not (is_directory (Store.release_dir target ~digest:state.Store.digest))
      then
        add "doctor.release"
          (Printf.sprintf "release '%s' is missing" state.Store.digest)
      else (
        match Store.verify_release target ~digest:state.Store.digest with
        | Ok () -> ()
        | Error message -> add "doctor.release" message);
      (* A staged state that matches current means the previous sync crashed
         after the swap but before creating entrypoints. The next sync repairs
         them; report the release as current rather than hand-edited. *)
      if not pending_state then
        List.iter
          (fun entrypoint ->
            let path = Filename.concat target.Store.root entrypoint in
            let expected = Store.entrypoint_link ~entrypoint in
            match Store.read_symlink path with
            | Ok (Some link) when link = expected ->
                if not (Store.file_exists (Filename.concat target.Store.managed
                                             ("current/" ^ entrypoint)))
                then
                  add "doctor.entrypoint"
                    (Printf.sprintf
                       "entrypoint '%s' does not resolve into the selected \
                        release"
                       entrypoint)
            | Ok _ ->
                add "doctor.entrypoint"
                  (Printf.sprintf
                     "entrypoint '%s' is not the stable managed symlink"
                     entrypoint)
            | Error message -> add "doctor.entrypoint" message)
          expected_entrypoints);
  let issues = List.sort (fun left right ->
      let by_code = String.compare left.code right.code in
      if by_code <> 0 then by_code else String.compare left.message right.message)
      !issues
  in
  {
    state;
    current;
    managed_exists;
    issues;
    integrity =
      (match issues with
      | [] -> Ok ()
      | issue :: _ -> Error issue.message);
  }

let policy_looking root =
  let target = Store.target_of_root root in
  let candidates =
    [ "AGENTS.md"; "AGENTS.fragment.md"; "opencode.json"; "framework.md";
      "persona.md"; Lock.lock_file; ".agents/skills"; ".opencode/skills";
      "skills" ]
  in
  List.exists
    (fun name -> Store.file_exists (Filename.concat root name))
    candidates
  || (is_directory target.Store.managed
     && List.exists
          (fun name -> name <> "releases")
          (Store.list_dir target.Store.managed))

let coverage_of_installed root state =
  let target = Store.target_of_root root in
  let path =
    Filename.concat
      (Store.release_dir target ~digest:state.Store.digest)
      ".nomia/explain.json"
  in
  match Store.read_file path with
  | Error _ -> empty_coverage
  | Ok text -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error _ -> empty_coverage
      | json -> coverage_of_explain json)

type expected = {
  digest : string option;
  source_digest : string;
  client : string option;
  role : string option;
  coverage : coverage;
  stale_hint : string option;
}

let expected_of_candidate ?(stale_hint = None) (policy_set : Compose.t) =
  let role =
    Option.map (fun (role : Model.role) -> role.name) policy_set.Compose.role
  in
  {
    digest = Some policy_set.Compose.digest;
    source_digest = policy_set.Compose.source_digest;
    client = Some policy_set.Compose.adapter.Model.id;
    role;
    coverage = coverage_of_rules policy_set.Compose.rules;
    stale_hint;
  }

let expected_for_repo source repo project inherit_root role_override =
  match Lock.find_in_repo ~repo with
  | Error diagnostics -> Error (Diagnostics diagnostics)
  | Ok lock ->
      let role =
        match role_override with
        | Some _ -> role_override
        | None -> (match lock with Some lock -> lock.Lock.role | None -> None)
      in
      let client =
        match lock with Some lock -> lock.Lock.client | None -> None
      in
      (match
         Compose.resolve ~repo ?project ~inherit_root ?role ?client ?today:None
           source
       with
      | Ok policy_set -> Ok (expected_of_candidate policy_set)
      | Error diagnostics
        when List.for_all
               (fun (diagnostic : Model.diagnostic) ->
                 starts_with ~prefix:"compose.lock." diagnostic.code)
               diagnostics ->
          let client = Option.value client ~default:"generic-agents" in
          (match
             Compose.resolve ~repo ?project ~inherit_root ?role ~client
               ~ignore_lock:true ?today:None source
           with
          | Error diagnostics -> Error (Diagnostics diagnostics)
          | Ok policy_set ->
              Ok
                (expected_of_candidate
                   ~stale_hint:
                     (Some
                        "the pinned release differs from the selected source; \
                         run sync --repo with --update-pin after review")
                   policy_set))
      | Error diagnostics -> Error (Diagnostics diagnostics))

let expected_for_client source client =
  match Compose.resolve ~client ?today:None source with
  | Error diagnostics -> Error (Diagnostics diagnostics)
  | Ok policy_set -> Ok (expected_of_candidate policy_set)

let cached_release () =
  let rec loop = function
    | [] -> None
    | digest :: rest -> (
        match Store.load_cached ~digest with
        | Ok cached -> Some (digest, cached)
        | Error _ -> loop rest)
  in
  loop (Store.cache_digests ())

let doctor_state_name = function
  | Current -> "current"
  | Stale -> "stale"
  | Missing -> "missing"
  | Unmanaged -> "unmanaged"
  | Hand_edited -> "hand-edited"
  | Partial -> "partial"
  | Offline_last_known_good -> "offline-last-known-good"
  | Source_unavailable -> "source-unavailable"

let option_json = function None -> `Null | Some value -> `String value
let strings values = `List (List.map (fun value -> `String value) values)

let coverage_json (coverage : coverage) =
  `Assoc
    [
      ("rules", `Int coverage.rules);
      ("supported", `Int coverage.supported);
      ("partial", `Int coverage.partial);
      ("unsupported", `Int coverage.unsupported);
      ("unsupported_rules", strings coverage.unsupported_rules);
      ("unsupported_controls", strings coverage.unsupported_controls);
    ]

let issue_json (issue : issue) =
  `Assoc [ ("code", `String issue.code); ("message", `String issue.message) ]

let doctor_to_json (report : report) =
  `Assoc
    [
      ("schema", `String "nomia-doctor/v1");
      ("state", `String (doctor_state_name report.state));
      ("target", `String report.target_kind);
      ("client", option_json report.client);
      ("project", option_json report.project);
      ("inherit_root", `Bool report.inherit_root);
      ("role", option_json report.role);
      ( "installed",
        `Assoc
          [
            ("release", option_json report.installed_release);
            ("digest", option_json report.installed_digest);
            ("source_digest", option_json report.installed_source_digest);
          ] );
      ( "source",
        `Assoc
          [
            ("available", `Bool report.source_available);
            ("release", option_json report.source_release);
            ("digest", option_json report.source_digest);
            ("source_digest", option_json report.source_files_digest);
          ] );
      ("cached", `Assoc [ ("digests", strings report.cached_digests) ]);
      ("entrypoints", strings report.entrypoints);
      ("enforcement", coverage_json report.coverage);
      ("issues", `List (List.map issue_json report.issues));
      ("warning", option_json report.warning);
    ]

let doctor_text (report : report) =
  let buffer = Buffer.create 1024 in
  let line format =
    Printf.ksprintf
      (fun text ->
        Buffer.add_string buffer text;
        Buffer.add_char buffer '\n')
      format
  in
  let digest name = function
    | None -> name ^ ": (none)"
    | Some value -> name ^ ": " ^ value
  in
  line "nomia doctor";
  line "state: %s" (doctor_state_name report.state);
  line "target: %s" report.target_kind;
  line "client: %s"
    (Option.value report.client ~default:"(none)");
  line "project: %s" (Option.value report.project ~default:"(none)");
  line "inherit_root: %b" report.inherit_root;
  line "role: %s" (Option.value report.role ~default:"(none)");
  line "%s" (digest "installed digest" report.installed_digest);
  line "%s" (digest "installed source" report.installed_source_digest);
  line "installed release: %s"
    (Option.value report.installed_release ~default:"(none)");
  line "%s" (digest "source digest" report.source_digest);
  line "%s" (digest "source files" report.source_files_digest);
  line "source release: %s"
    (Option.value report.source_release ~default:"(unavailable)");
  line "entrypoints: %s"
    (match report.entrypoints with
    | [] -> "(none)"
    | values -> String.concat ", " values);
  line "enforcement: %d rules, %d supported, %d partial, %d unsupported"
    report.coverage.rules report.coverage.supported report.coverage.partial
    report.coverage.unsupported;
  if report.coverage.unsupported_controls <> [] then
    line "unsupported controls: %s"
      (String.concat ", " report.coverage.unsupported_controls);
  if report.coverage.unsupported_rules <> [] then
    line "unsupported rules: %s"
      (String.concat ", " report.coverage.unsupported_rules);
  line "cached: %s"
    (match report.cached_digests with
    | [] -> "(none)"
    | values -> String.concat ", " values);
  if report.issues <> [] then begin
    line "issues:";
    List.iter
      (fun issue -> line "  %s: %s" issue.code issue.message)
      report.issues
  end;
  Option.iter (fun warning -> line "warning: %s" warning) report.warning;
  Buffer.contents buffer

let doctor_exit (report : report) =
  match report.state with
  | Current | Partial | Offline_last_known_good -> 0
  | Source_unavailable -> 2
  | Stale | Missing | Unmanaged | Hand_edited -> 1

let report_of ~kind ~root ~source_root ~project ~inherit_root ~expected =
  let installed = installed_info root in
  let cached = Store.cache_digests () in
  let installed_digest =
    Option.map (fun (state : Store.state) -> state.digest) installed.state
  in
  let installed_release =
    Option.map (fun (state : Store.state) -> state.release) installed.state
  in
  let installed_source_digest =
    Option.map (fun (state : Store.state) -> state.source_digest) installed.state
  in
  let entrypoints =
    match installed.state with Some state -> state.Store.entrypoints | None -> []
  in
  let base_client =
    match expected with
    | Some expected -> expected.client
    | None -> Option.map (fun (state : Store.state) -> state.client) installed.state
  in
  let base_role =
    match expected with
    | Some expected -> expected.role
    | None -> Option.bind installed.state (fun (state : Store.state) -> state.role)
  in
  let make ~state ~source_available ~source_digest ~source_files_digest
      ~source_release ~coverage ~issues ~warning =
    {
      state;
      target_kind = kind;
      client = base_client;
      role = base_role;
      installed_digest;
      installed_release;
      installed_source_digest;
      source_available;
      source_digest;
      source_files_digest;
      source_release;
      project;
      inherit_root;
      cached_digests = cached;
      entrypoints;
      coverage;
      issues;
      warning;
    }
  in
  match source_root with
  | None -> (
      let fallback_cache = cached_release () in
      match installed.state with
      | Some state when installed.integrity = Ok () ->
          Ok
            (make ~state:Offline_last_known_good ~source_available:false
               ~source_digest:None ~source_files_digest:None
               ~source_release:None
               ~coverage:(coverage_of_installed root state)
               ~issues:[]
               ~warning:
                 (Some
                    "policy source is unavailable; keeping the installed \
                     last-known-good release"))
      | _ ->
          if installed.managed_exists && installed.integrity <> Ok () then
            Ok
              (make ~state:Hand_edited ~source_available:false
                 ~source_digest:None ~source_files_digest:None
                 ~source_release:None
                 ~coverage:
                   (match installed.state with
                   | Some state -> coverage_of_installed root state
                   | None -> empty_coverage)
                 ~issues:installed.issues
                 ~warning:(Some "policy source is unavailable"))
          else if policy_looking root then
            Ok
              (make ~state:Unmanaged ~source_available:false
                 ~source_digest:None ~source_files_digest:None
                 ~source_release:None ~coverage:empty_coverage
                 ~issues:
                   [
                     {
                       code = "doctor.unmanaged";
                       message =
                         "policy-looking files were not created by sync; move \
                          them into reviewed source and run sync";
                     };
                   ]
                 ~warning:(Some "policy source is unavailable"))
          else
            match fallback_cache with
            | Some (digest, cached) ->
                Ok
                  (make ~state:Offline_last_known_good ~source_available:false
                     ~source_digest:(Some digest) ~source_files_digest:None
                     ~source_release:None
                     ~coverage:(coverage_of_explain cached.Store.explain)
                     ~issues:[]
                     ~warning:
                       (Some
                          (Printf.sprintf
                             "policy source is unavailable; using cached \
                              last-known-good release %s"
                             digest)))
            | None ->
                Ok
                  (make ~state:Source_unavailable ~source_available:false
                     ~source_digest:None ~source_files_digest:None
                     ~source_release:None ~coverage:empty_coverage ~issues:[]
                     ~warning:
                       (Some
                          "policy source is unavailable and no cached release \
                           exists; refusing to substitute an empty policy")))
  | Some _ -> (
      match expected with
      | None ->
          Error
            (Message
               "internal doctor error: source is available but no expected \
                release was resolved")
      | Some expected -> (
          let source_digest = expected.digest in
          let source_files_digest = Some expected.source_digest in
          let coverage = expected.coverage in
          if not installed.managed_exists then
            let state = if policy_looking root then Unmanaged else Missing in
            let issues =
              match state with
              | Unmanaged ->
                  [
                    {
                      code = "doctor.unmanaged";
                      message =
                        "policy-looking files were not created by sync; move \
                         them into reviewed source and run sync";
                    };
                  ]
              | _ -> []
            in
            Ok
              (make ~state ~source_available:true ~source_digest
                 ~source_files_digest ~source_release:None ~coverage ~issues
                 ~warning:None)
          else if installed.integrity <> Ok () then
            Ok
              (make ~state:Hand_edited ~source_available:true ~source_digest
                 ~source_files_digest ~source_release:None ~coverage
                 ~issues:installed.issues ~warning:None)
          else
            match installed.state with
            | None ->
                Ok
                  (make ~state:Hand_edited ~source_available:true ~source_digest
                     ~source_files_digest ~source_release:None ~coverage
                     ~issues:installed.issues ~warning:None)
            | Some state ->
                let client_changed =
                  match (expected.client, state.Store.client) with
                  | Some expected_client, installed_client ->
                      expected_client <> installed_client
                  | None, _ -> false
                in
                let source_changed =
                  client_changed
                  || expected.stale_hint <> None
                  || expected.source_digest <> state.Store.source_digest
                  || expected.digest <> Some state.Store.digest
                in
                if source_changed then
                  Ok
                    (make ~state:Stale ~source_available:true ~source_digest
                       ~source_files_digest ~source_release:None ~coverage
                       ~issues:
                         [
                           {
                             code = "doctor.stale";
                             message =
                               "a newer release is available; review the source \
                                change and run sync (--update-pin for a \
                                repository) to install it";
                           };
                         ]
                       ~warning:None)
                else if
                  expected.coverage.partial > 0
                  || expected.coverage.unsupported > 0
                then
                  Ok
                    (make ~state:Partial ~source_available:true ~source_digest
                       ~source_files_digest ~source_release:None ~coverage
                       ~issues:[] ~warning:None)
                else
                  Ok
                    (make ~state:Current ~source_available:true ~source_digest
                       ~source_files_digest ~source_release:None ~coverage
                       ~issues:[] ~warning:None)))

let fill_source_release ~source_root report =
  match source_root with
  | None -> report
  | Some root -> (
      match load_source root with
      | Error _ -> report
      | Ok source ->
          {
            report with
            source_release = Some source.Model.policy.release;
          })

let doctor ~repo ~project ~inherit_root ~role ~client ~source_root =
  match (repo, client) with
  | Some repo, None ->
      let target_root =
        match project with
        | None -> repo
        | Some project -> Filename.concat repo project
      in
      let expected =
        match source_root with
        | None -> Ok None
        | Some root -> (
            match load_source root with
            | Error diagnostics -> Error (Diagnostics diagnostics)
            | Ok source -> (
                match
                  expected_for_repo source repo project inherit_root role
                with
                | Error error -> Error error
                | Ok expected -> Ok (Some expected)))
      in
      (match expected with
      | Error error -> Error error
      | Ok expected ->
          (match
             report_of ~kind:"repo" ~root:target_root ~source_root ~project
               ~inherit_root ~expected
           with
          | Error error -> Error error
          | Ok report -> Ok (fill_source_release ~source_root report)))
  | None, Some client -> (
      if project <> None then
        Error
          (Message
             "--project only applies to a repository target; user-level doctor \
              has no project context")
      else
      let root = Store.client_target ~client in
      let expected =
        match source_root with
        | None -> Ok None
        | Some source_root -> (
            match load_source source_root with
            | Error diagnostics -> Error (Diagnostics diagnostics)
            | Ok source -> (
                match expected_for_client source client with
                | Error error -> Error error
                | Ok expected -> Ok (Some expected)))
      in
      match expected with
      | Error error -> Error error
      | Ok expected ->
          (match
             report_of ~kind:"client" ~root ~source_root ~project:None
               ~inherit_root:false ~expected
           with
          | Error error -> Error error
          | Ok report -> Ok (fill_source_release ~source_root report)))
  | None, None ->
      Error
        (Message
           "doctor needs a target: pass --repo DIR or --client CLIENT")
  | Some _, Some _ ->
      Error
        (Message
           "doctor takes exactly one target: --repo DIR or --client CLIENT")
