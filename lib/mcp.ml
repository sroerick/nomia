(** A read-only Model Context Protocol adapter over one pinned release. *)

let protocol_version = "2024-11-05"
let server_name = "agentctl-mcp"

(* ------------------------------------------------------------------ *)
(* Small helpers                                                       *)
(* ------------------------------------------------------------------ *)

let starts_with ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.sub value 0 prefix_len = prefix

let find_sub ?(from = 0) text needle =
  let text_len = String.length text and needle_len = String.length needle in
  if needle_len = 0 then Some from
  else
    let rec loop index =
      if index + needle_len > text_len then None
      else if String.sub text index needle_len = needle then Some index
      else loop (index + 1)
    in
    loop (max 0 from)

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let string_member key json =
  match member key json with Some (`String value) -> Some value | _ -> None

let list_member key json =
  match member key json with Some (`List values) -> Some values | _ -> None

let json_rpc_notification = function
  | `Assoc entries -> not (List.mem_assoc "id" entries)
  | _ -> true

let diagnostic_line (diagnostic : Model.diagnostic) =
  match diagnostic.line with
  | Some line ->
      Printf.sprintf "%s:%d: %s: %s" diagnostic.path line diagnostic.code
        diagnostic.message
  | None ->
      Printf.sprintf "%s: %s: %s" diagnostic.path diagnostic.code
        diagnostic.message

let diagnostics_message diagnostics =
  String.concat "; " (List.map diagnostic_line diagnostics)

let report message = prerr_endline ("agentctl mcp: " ^ message)

(* ------------------------------------------------------------------ *)
(* Pinned release                                                      *)
(* ------------------------------------------------------------------ *)

type resource = {
  uri : string;
  name : string;
  mime : string;
  description : string;
  text : string;
}

type pinned = {
  release : string;
  digest : string;
  client : string;
  role : string option;
  repo : string option;
  project : string option;
  manifest_json : Yojson.Safe.t;
  manifest_text : string;
  explain_json : Yojson.Safe.t;
  explain_text : string;
  core_text : string;
  capabilities_json : Yojson.Safe.t;
  rule_texts : (string * string) list;  (** [(rule id, canonical JSON + LF)]. *)
  skill_texts : (string * string) list;  (** [(skill name, SKILL.md bytes)]. *)
  resources : resource list;
}

let canonical_text json = Canonical.to_string json ^ "\n"

let artifact_text path artifacts =
  List.find_map
    (fun (candidate, contents, _) ->
      if candidate = path then Some contents else None)
    artifacts

let skill_dir_of explain ~fallback =
  match member "capabilities" explain with
  | Some capabilities -> (
      match member "skills" capabilities with
      | Some skills -> (
          match member "dir" skills with
          | Some (`String dir) -> dir
          | _ -> fallback)
      | None -> fallback)
  | None -> fallback

let rule_metadata explain id =
  list_member "rules" explain
  |> Option.value ~default:[]
  |> List.find_opt (fun json -> string_member "id" json = Some id)

let rule_document ~release ~digest ~id metadata body =
  `Assoc
    [
      ("schema", `String "nomia-mcp-rule/v1");
      ("release", `String release);
      ("digest", `String digest);
      ("id", `String id);
      ("metadata", metadata);
      ("body", `String body);
      ("body_sha256", `String (Canonical.sha256_hex body));
    ]

let capability_document ~release ~digest ~client capabilities =
  `Assoc
    [
      ("schema", `String "nomia-mcp-capabilities/v1");
      ("release", `String release);
      ("digest", `String digest);
      ("client", `String client);
      ("capabilities", capabilities);
    ]

let skill_texts_of_artifacts ~skill_dir explain artifacts =
  let names =
    list_member "skills" explain
    |> Option.value ~default:[]
    |> List.filter_map (fun json -> string_member "name" json)
  in
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | name :: rest -> (
        let path = skill_dir ^ "/" ^ name ^ "/SKILL.md" in
        match artifact_text path artifacts with
        | Some text -> collect ((name, text) :: acc) rest
        | None ->
            Error
              (Printf.sprintf
                 "pinned release is missing the rendered skill file '%s'" path))
  in
  collect [] names

let build_resources pinned =
  let base = "org-policy://releases/" ^ pinned.release in
  let manifest =
    {
      uri = base ^ "/manifest";
      name = "manifest";
      mime = "application/json";
      description = "Canonical release manifest";
      text = pinned.manifest_text;
    }
  in
  let core =
    {
      uri = base ^ "/core";
      name = "core";
      mime = "text/markdown";
      description = "Organization-only rule text";
      text = pinned.core_text;
    }
  in
  let explain =
    {
      uri = base ^ "/explain";
      name = "explain";
      mime = "application/json";
      description = "Canonical explain document";
      text = pinned.explain_text;
    }
  in
  let rules =
    pinned.rule_texts
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.map (fun (id, text) ->
           {
             uri = base ^ "/rules/" ^ id;
             name = "rule:" ^ id;
             mime = "application/json";
             description = "Rule metadata and body";
             text;
           })
  in
  let skills =
    pinned.skill_texts
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.map (fun (name, text) ->
           {
             uri = base ^ "/skills/" ^ name;
             name = "skill:" ^ name;
             mime = "text/markdown";
             description = "Skill SKILL.md bytes";
             text;
           })
  in
  let capabilities =
    {
      uri = "org-policy://capabilities/" ^ pinned.client;
      name = "capabilities:" ^ pinned.client;
      mime = "application/json";
      description = "Adapter capability matrix";
      text =
        canonical_text
          (capability_document ~release:pinned.release ~digest:pinned.digest
             ~client:pinned.client pinned.capabilities_json);
    }
  in
  manifest :: core :: explain :: (rules @ skills @ [ capabilities ])

let assemble ~release ~digest ~client ~role ~repo ~project ~manifest_json
    ~manifest_text ~explain_json ~core_text ~capabilities_json ~rule_texts
    ~skill_texts =
  let base =
    {
      release;
      digest;
      client;
      role;
      repo;
      project;
      manifest_json;
      manifest_text;
      explain_json;
      explain_text = canonical_text explain_json;
      core_text;
      capabilities_json;
      rule_texts;
      skill_texts;
      resources = [];
    }
  in
  { base with resources = build_resources base }

(* ------------------------------------------------------------------ *)
(* Release construction: live source                                   *)
(* ------------------------------------------------------------------ *)

let manifest_of_policy_set (policy_set : Compose.t) artifacts =
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
    List.map (fun (exception_ : Model.exception_) -> exception_.id)
      policy_set.Compose.exceptions
  in
  let artifacts =
    List.map
      (fun (artifact : Render.artifact) -> (artifact.path, artifact.contents))
      artifacts
  in
  Release.build ~client:policy_set.Compose.adapter.Model.id ?role
    ?project:policy_set.Compose.project ~fragments ~exceptions ~artifacts
    policy_set.Compose.source

let pinned_of_policy_set ~repo (policy_set : Compose.t) artifacts =
  let release = policy_set.Compose.source.Model.policy.release in
  let digest = policy_set.Compose.digest in
  let manifest = manifest_of_policy_set policy_set artifacts in
  if Release.digest manifest <> digest then
    Error
      "internal release digest mismatch between composition and manifest"
  else
    let explain = Compose.explain_json policy_set in
    let capabilities =
      match member "capabilities" explain with Some value -> value | None -> `Null
    in
    let skill_dir =
      skill_dir_of explain ~fallback:policy_set.Compose.adapter.Model.skills.dir
    in
    let rec rule_texts acc = function
      | [] -> Ok (List.rev acc)
      | (resolved : Compose.resolved_rule) :: rest -> (
          let id = resolved.rule.Model.id in
          match rule_metadata explain id with
          | None ->
              Error
                (Printf.sprintf
                   "internal explain document is missing rule metadata for '%s'"
                   id)
          | Some metadata ->
              let body = String.trim resolved.rule.Model.body in
              let document = rule_document ~release ~digest ~id metadata body in
              rule_texts ((id, canonical_text document) :: acc) rest)
    in
    match rule_texts [] policy_set.Compose.rules with
    | Error message -> Error message
    | Ok rule_texts -> (
        let artifact_triples =
          List.map
            (fun (artifact : Render.artifact) ->
              (artifact.path, artifact.contents, artifact.mode))
            artifacts
        in
        match
          skill_texts_of_artifacts ~skill_dir explain artifact_triples
        with
        | Error message -> Error message
        | Ok skill_texts ->
            Ok
              (assemble ~release ~digest ~client:policy_set.Compose.adapter.id
                 ~role:
                   (Option.map
                      (fun (role : Model.role) -> role.name)
                      policy_set.Compose.role)
                 ~repo ~project:policy_set.Compose.project
                 ~manifest_json:(Release.manifest_json manifest)
                 ~manifest_text:(Release.to_string manifest ^ "\n")
                 ~explain_json:explain
                 ~core_text:(Render_erde.org_core policy_set)
                 ~capabilities_json:capabilities ~rule_texts ~skill_texts))

(* ------------------------------------------------------------------ *)
(* Release construction: cached fallback                               *)
(* ------------------------------------------------------------------ *)

let truncate_at_markers text markers =
  let indices = List.filter_map (fun marker -> find_sub text marker) markers in
  match indices with
  | [] -> text
  | first :: rest ->
      let cut = List.fold_left min first rest in
      String.sub text 0 cut ^ "\n"

let cached_core artifacts =
  match artifact_text "org-core.md" artifacts with
  | Some text -> Ok text
  | None -> (
      let core_markers =
        [
          "\n\n# Repository instructions (";
          "\n\n# Role: ";
          "\n\n# Skills";
        ]
      in
      match artifact_text "AGENTS.md" artifacts with
      | Some text ->
          (* The organization core is exactly the leading section of the
             generated AGENTS.md; later sections begin at a known heading. *)
          Ok (truncate_at_markers text core_markers)
      | None -> (
          match artifact_text "framework.md" artifacts with
          | Some text -> Ok (truncate_at_markers text [ "\n\n## Shared harness" ])
          | None -> (
              match artifact_text "persona.md" artifacts with
              | Some text -> Ok (truncate_at_markers text [ "\n\n## Voice" ])
              | None ->
                  Error
                    "cached release has no organization core artifact"))
      )

let rule_body_from_text text id =
  let needle = "\n## " ^ id ^ " (" in
  match find_sub text needle with
  | None -> None
  | Some index ->
      let after_heading = index + String.length needle in
      let body_start =
        match String.index_from_opt text after_heading '\n' with
        | Some newline -> newline + 1
        | None -> String.length text
      in
      let rest =
        String.sub text body_start (String.length text - body_start)
      in
      let boundaries =
        List.filter_map
          (fun marker -> find_sub rest marker)
          [ "\n## "; "\n\n# " ]
      in
      let body =
        match boundaries with
        | [] -> rest
        | first :: remaining ->
            let cut = List.fold_left min first remaining in
            String.sub rest 0 cut
      in
      Some (String.trim body)

let cached_rule_texts ~release ~digest explain text =
  let rules =
    list_member "rules" explain |> Option.value ~default:[]
  in
  let rec collect acc missing = function
    | [] -> Ok (List.rev acc, List.rev missing)
    | metadata :: rest -> (
        match string_member "id" metadata with
        | None ->
            Error "cached release explain document has a rule without an id"
        | Some id -> (
            match rule_body_from_text text id with
            | None -> collect acc (id :: missing) rest
            | Some body ->
                let document = rule_document ~release ~digest ~id metadata body in
                collect ((id, canonical_text document) :: acc) missing rest))
  in
  collect [] [] rules

let cached_render_text artifacts =
  match artifact_text "AGENTS.md" artifacts with
  | Some text -> Ok text
  | None -> (
      match artifact_text "framework.md" artifacts with
      | Some text -> Ok text
      | None -> (
          match artifact_text "org-core.md" artifacts with
          | Some text -> Ok text
          | None -> (
              match artifact_text "persona.md" artifacts with
              | Some text -> Ok text
              | None ->
                  Error "cached release has no rendered policy text to read")))

let pinned_of_cached ~repo (cached : Store.cached) =
  match string_member "digest" cached.manifest with
  | None -> Error "cached release manifest has no digest"
  | Some digest -> (
      match string_member "release" cached.manifest with
      | None -> Error "cached release manifest has no release version"
      | Some release -> (
          let explain = cached.explain in
          let client =
            match member "adapter" explain with
            | Some adapter -> (
                match string_member "id" adapter with
                | Some id -> id
                | None -> "unknown")
            | None -> "unknown"
          in
          let role = string_member "role" explain in
          let project = string_member "project" explain in
          let capabilities =
            match member "capabilities" explain with
            | Some value -> value
            | None -> `Null
          in
          let skill_dir = skill_dir_of explain ~fallback:".agents/skills" in
          match cached_core cached.artifacts with
          | Error message -> Error message
          | Ok core_text -> (
              match cached_render_text cached.artifacts with
              | Error message -> Error message
              | Ok render_text -> (
                  match
                    cached_rule_texts ~release ~digest explain render_text
                  with
                  | Error message -> Error message
                  | Ok (rule_texts, missing_rules) -> (
                      List.iter
                        (fun id ->
                          report
                            (Printf.sprintf
                               "cached release %s does not contain the body of \
                                rule '%s' in its rendered artifacts; that rule \
                                resource is omitted"
                               digest id))
                        missing_rules;
                      match
                        skill_texts_of_artifacts ~skill_dir explain
                          cached.artifacts
                      with
                      | Error message -> Error message
                      | Ok skill_texts ->
                          Ok
                            (assemble ~release ~digest ~client ~role ~repo
                               ~project ~manifest_json:cached.manifest
                               ~manifest_text:
                                 (Canonical.to_string cached.manifest ^ "\n")
                               ~explain_json:explain ~core_text
                               ~capabilities_json:capabilities ~rule_texts
                               ~skill_texts))))))

(* ------------------------------------------------------------------ *)
(* Startup selection                                                   *)
(* ------------------------------------------------------------------ *)

let source_available root =
  Store.file_exists (Filename.concat root "policy.yaml")

let rec walk_for_policy dir =
  let candidate = Filename.concat dir "nomia/policy.yaml" in
  if Sys.file_exists candidate then Some (Filename.concat dir "nomia")
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else walk_for_policy parent

let discover_source source =
  let nonempty = function
    | Some value when String.trim value <> "" -> Some value
    | _ -> None
  in
  match nonempty source with
  | Some root -> Some root
  | None -> (
      match nonempty (Sys.getenv_opt "NOMIA_SOURCE") with
      | Some root -> Some root
      | None -> (
          let cwd = Sys.getcwd () in
          if Sys.file_exists (Filename.concat cwd "policy.yaml") then Some cwd
          else walk_for_policy cwd))

let compare_release left right =
  let parts value =
    String.split_on_char '.' value |> List.map int_of_string_opt
  in
  let rec loop left right =
    match (left, right) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
        let x = Option.value x ~default:0 and y = Option.value y ~default:0 in
        let by_part = Int.compare x y in
        if by_part <> 0 then by_part else loop xs ys
  in
  loop (parts left) (parts right)

let cached_client (cached : Store.cached) =
  match member "client" cached.manifest with
  | Some (`String client) -> client
  | _ -> (
      match member "adapter" cached.explain with
      | Some adapter -> (
          match string_member "id" adapter with Some id -> id | None -> "unknown")
      | None -> "unknown")

let cached_release (cached : Store.cached) =
  match string_member "release" cached.manifest with
  | Some release -> release
  | None -> "0.0.0"

let cache_candidates ~client =
  Store.cache_digests ()
  |> List.filter_map (fun digest ->
         match Store.load_cached ~digest with
         | Error message ->
             report
               (Printf.sprintf "ignoring unreadable cached release %s: %s"
                  digest message);
             None
         | Ok cached ->
             let cached_client = cached_client cached in
             match client with
             | Some wanted when wanted <> cached_client ->
                 report
                   (Printf.sprintf
                      "ignoring cached release %s for client %s while selecting \
                       client %s"
                      digest cached_client wanted);
                 None
             | _ -> Some (cached_release cached, digest, cached))

let check_context ~client ~project pinned =
  match client with
  | Some wanted when pinned.client <> wanted ->
      Error
        (Printf.sprintf "release belongs to client %s, not %s" pinned.client
           wanted)
  | _ -> (
      match project with
      | Some wanted when pinned.project <> Some wanted ->
          Error
            (Printf.sprintf "release belongs to project %s, not %s"
               (Option.value pinned.project ~default:"(none)")
               wanted)
      | _ -> Ok pinned)

let select_cached ~repo ~client ~project =
  let candidates = cache_candidates ~client in
  let candidates =
    List.sort
      (fun (left_release, left_digest, _) (right_release, right_digest, _) ->
        let by_release = compare_release right_release left_release in
        if by_release <> 0 then by_release
        else String.compare right_digest left_digest)
      candidates
  in
  let installed_fallback () =
    let root =
      match repo with
      | Some repo -> Some repo
      | None ->
          Option.map
            (fun client -> Store.client_target ~client)
            client
    in
    match root with
    | None -> Error "no installed target is selected for the offline fallback"
    | Some root -> (
        let target = Store.target_of_root root in
        match Store.current_digest target with
        | Error message -> Error message
        | Ok None -> Error "the installed target has no current release"
        | Ok (Some digest) -> (
            match Store.load_installed ~root ~digest with
            | Error message -> Error message
            | Ok cached -> (
                match pinned_of_cached ~repo cached with
                | Error message -> Error message
                | Ok pinned -> (
                    match check_context ~client ~project pinned with
                    | Error message ->
                        Error
                          (Printf.sprintf
                             "installed release %s is not suitable: %s" digest
                             message)
                    | Ok pinned ->
                        report
                          (Printf.sprintf
                             "source and cache unavailable; serving installed \
                              release %s (client %s)"
                             digest pinned.client);
                        Ok pinned))))
  in
  let rec loop = function
    | [] -> (
        match installed_fallback () with
        | Ok pinned -> Ok pinned
        | Error _ ->
            Error
              "no policy source is available and no suitable cached or \
               installed release exists; run 'agentctl sync --client CLIENT' \
               while the source is reachable")
    | (release, digest, cached) :: rest -> (
        match pinned_of_cached ~repo cached with
        | Error message ->
            report
              (Printf.sprintf
                 "ignoring unusable cached release %s: %s" digest message);
            loop rest
        | Ok pinned -> (
            match check_context ~client ~project pinned with
            | Error message ->
                report
                  (Printf.sprintf
                     "ignoring cached release %s for this context: %s" digest
                     message);
                loop rest
            | Ok pinned ->
                report
                  (Printf.sprintf
                     "source unavailable; serving cached release %s (%s, client \
                      %s)"
                     release digest pinned.client);
                Ok pinned))
  in
  loop candidates

let select_release ~repo ~client ~project digest =
  if not (Store.valid_digest digest) then
    Error "release must be 'sha256:' plus 64 lowercase hex characters"
  else
    match Store.load_cached ~digest with
    | Error message ->
        Error
          (Printf.sprintf
             "cached release %s is missing or incomplete; run 'agentctl sync \
              --client CLIENT' while the source is reachable (%s)"
             digest message)
    | Ok cached -> (
        let cached_client = cached_client cached in
        match client with
        | Some wanted when wanted <> cached_client ->
            Error
              (Printf.sprintf
                 "cached release %s belongs to client %s, not %s" digest
                 cached_client wanted)
        | _ -> (
            match pinned_of_cached ~repo cached with
            | Error message -> Error message
            | Ok pinned -> check_context ~client ~project pinned))

let select_pinned ~source ~repo ~role ~client ~project ~release =
  match release with
  | Some digest -> select_release ~repo ~client ~project digest
  | None -> (
      match discover_source source with
      | Some root when source_available root -> (
          match
            Compose.resolve_root ?repo ?project ?role ?client ?today:None ~root
              ()
          with
          | Error diagnostics -> Error (diagnostics_message diagnostics)
          | Ok policy_set -> (
              let client_name = policy_set.Compose.adapter.Model.id in
              match Render.render policy_set ~client:client_name with
              | Error diagnostics -> Error (diagnostics_message diagnostics)
              | Ok artifacts -> pinned_of_policy_set ~repo policy_set artifacts))
      | _ -> select_cached ~repo ~client ~project)

(* ------------------------------------------------------------------ *)
(* JSON-RPC                                                            *)
(* ------------------------------------------------------------------ *)

type outcome = Continue | Stop

let respond json =
  output_string stdout (Canonical.to_string json);
  output_char stdout '\n';
  flush stdout

let respond_error id code message =
  respond
    (`Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", match id with Some value -> value | None -> `Null);
        ( "error",
          `Assoc [ ("code", `Int code); ("message", `String message) ] );
      ])

let respond_result id result =
  respond
    (`Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", match id with Some value -> value | None -> `Null);
        ("result", result);
      ])

let initialize_result () =
  `Assoc
    [
      ("protocolVersion", `String protocol_version);
      ( "capabilities",
        `Assoc
          [
            ( "resources",
              `Assoc
                [
                  ("subscribe", `Bool false);
                  ("listChanged", `Bool false);
                ] );
            ("tools", `Assoc [ ("listChanged", `Bool false) ]);
          ] );
      ( "serverInfo",
        `Assoc [ ("name", `String server_name); ("version", `String "0.1.0") ]
      );
    ]

let resource_json (resource : resource) =
  `Assoc
    [
      ("uri", `String resource.uri);
      ("name", `String resource.name);
      ("mimeType", `String resource.mime);
      ("description", `String resource.description);
    ]

let resources_list pinned =
  `Assoc
    [ ("resources", `List (List.map resource_json pinned.resources)) ]

let read_resource pinned params =
  match params with
  | `Assoc entries -> (
      match List.assoc_opt "uri" entries with
      | Some (`String uri) -> (
          if starts_with ~prefix:"org-policy://releases/" uri then
            let rest =
              String.sub uri (String.length "org-policy://releases/")
                (String.length uri - String.length "org-policy://releases/")
            in
            match String.split_on_char '/' rest with
            | release :: _ when release <> pinned.release ->
                Error
                  (Printf.sprintf
                     "requested release '%s' is not the pinned release '%s'; a \
                      running server never changes releases"
                     release pinned.release)
            | _ -> (
                match
                  List.find_opt
                    (fun (resource : resource) -> resource.uri = uri)
                    pinned.resources
                with
                | Some resource -> Ok resource
                | None -> Error (Printf.sprintf "unknown resource '%s'" uri))
          else if starts_with ~prefix:"org-policy://capabilities/" uri then
            let client =
              String.sub uri (String.length "org-policy://capabilities/")
                (String.length uri - String.length "org-policy://capabilities/")
            in
            if client <> pinned.client then
              Error
                (Printf.sprintf
                   "capabilities for client '%s' are not part of the pinned \
                    release '%s' (client '%s')"
                   client pinned.release pinned.client)
            else
              match
                List.find_opt
                  (fun (resource : resource) -> resource.uri = uri)
                  pinned.resources
              with
              | Some resource -> Ok resource
              | None -> Error (Printf.sprintf "unknown resource '%s'" uri)
          else Error (Printf.sprintf "unknown resource '%s'" uri))
      | Some _ -> Error "resources/read params.uri must be a string"
      | None -> Error "resources/read requires params.uri")
  | _ -> Error "resources/read params must be an object"

let read_resource_result (resource : resource) =
  `Assoc
    [
      ( "contents",
        `List
          [
            `Assoc
              [
                ("uri", `String resource.uri);
                ("mimeType", `String resource.mime);
                ("text", `String resource.text);
              ];
          ] );
    ]

let tools_list () =
  `Assoc
    [
      ( "tools",
        `List
          [
            `Assoc
              [
                ("name", `String "resolve_policy");
                ( "description",
                  `String
                    "Return the pinned release manifest, organization core, \
                     explain document, and capability summary" );
                ( "inputSchema",
                  `Assoc
                    [
                      ("type", `String "object");
                      ( "properties",
                        `Assoc
                          [
                            ( "client",
                              `Assoc [ ("type", `String "string") ] );
                            ("role", `Assoc [ ("type", `String "string") ]);
                            ("repo", `Assoc [ ("type", `String "string") ]);
                            ( "release",
                              `Assoc [ ("type", `String "string") ] );
                          ] );
                      ("additionalProperties", `Bool false);
                    ] );
              ];
          ] );
    ]

let tool_payload pinned =
  let release_node =
    match member "release" pinned.explain_json with
    | Some (`Assoc _ as node) -> node
    | _ -> `Assoc [ ("version", `String pinned.release) ]
  in
  `Assoc
    [
      ("schema", `String "nomia-mcp-resolve/v1");
      ("release", release_node);
      ("digest", `String pinned.digest);
      ( "source_digest",
        match member "source_digest" pinned.explain_json with
        | Some value -> value
        | None -> `Null );
      ("client", `String pinned.client);
      ( "role",
        match pinned.role with Some role -> `String role | None -> `Null );
      ( "fragment",
        match member "fragment" pinned.explain_json with
        | Some value -> value
        | None -> `Null );
      ("manifest", pinned.manifest_json);
      ("core", `String pinned.core_text);
      ("explain", pinned.explain_json);
      ("capabilities", pinned.capabilities_json);
    ]

let tool_result payload =
  `Assoc
    [
      ( "content",
        `List
          [
            `Assoc
              [
                ("type", `String "text");
                ("text", `String (Canonical.to_string payload ^ "\n"));
              ];
          ] );
      ("structuredContent", payload);
      ("isError", `Bool false);
    ]

let validate_tool_arguments pinned arguments =
  let allowed = [ "client"; "role"; "repo"; "project"; "release" ] in
  match arguments with
  | `Assoc fields ->
      let rec check = function
        | [] -> Ok ()
        | (key, value) :: rest ->
            if not (List.mem key allowed) then
              Error
                (Printf.sprintf
                   "unsupported resolve_policy argument '%s'; only client, \
                    role, repo, project, and release are accepted"
                   key)
            else
              match value with
              | `String text -> (
                  match key with
                  | "release" ->
                      if text = pinned.release || text = pinned.digest then
                        check rest
                      else
                        Error
                          (Printf.sprintf
                             "requested release '%s' is not the pinned release \
                              '%s'; a tool call cannot change releases"
                             text pinned.release)
                  | "client" ->
                      if text = pinned.client then check rest
                      else
                        Error
                          (Printf.sprintf
                             "requested client '%s' is not the pinned client \
                              '%s'"
                             text pinned.client)
                  | "role" -> (
                      match pinned.role with
                      | Some role when role = text -> check rest
                      | Some role ->
                          Error
                            (Printf.sprintf
                               "requested role '%s' is not the pinned role '%s'"
                               text role)
                      | None ->
                          Error
                            "this server is pinned without a role; a tool call \
                             cannot select one")
                  | "project" -> (
                      match pinned.project with
                      | Some project when project = text -> check rest
                      | Some project ->
                          Error
                            (Printf.sprintf
                               "requested project '%s' is not the pinned \
                                project '%s'"
                               text project)
                      | None ->
                          Error
                            "this server is pinned without a project; a tool \
                             call cannot select one")
                  | "repo" -> (
                      match pinned.repo with
                      | Some repo when repo = text -> check rest
                      | Some _ ->
                          Error
                            "this server is pinned to a different repository \
                             context"
                      | None ->
                          Error
                            "this server is pinned without a repository \
                             context; a tool call cannot select one")
                  | _ -> assert false)
              | _ ->
                  Error
                    (Printf.sprintf
                       "resolve_policy argument '%s' must be a string" key)
      in
      check fields
  | _ -> Error "tools/call arguments must be an object"

let call_tool pinned params =
  match params with
  | `Assoc entries -> (
      match List.assoc_opt "name" entries with
      | Some (`String "resolve_policy") -> (
          let arguments =
            match List.assoc_opt "arguments" entries with
            | None -> `Assoc []
            | Some value -> value
          in
          match validate_tool_arguments pinned arguments with
          | Error message -> Error message
          | Ok () -> Ok (tool_result (tool_payload pinned)))
      | Some (`String other) ->
          Error
            (Printf.sprintf
               "unknown tool '%s'; this server exposes only resolve_policy" other)
      | Some _ -> Error "tools/call params.name must be a string"
      | None -> Error "tools/call requires params.name")
  | _ -> Error "tools/call params must be an object"

let handle pinned json : outcome =
  match json with
  | `Assoc _ ->
      let id = member "id" json in
      let is_notification = id = None in
      let method_ = string_member "method" json in
      let params =
        match member "params" json with Some value -> value | None -> `Null
      in
      (match method_ with
      | None ->
          if is_notification then Continue
          else begin
            respond_error id (-32600) "invalid JSON-RPC request";
            Continue
          end
      | Some "exit" -> Stop
      | Some "notifications/initialized" -> Continue
      | Some _ when is_notification ->
          (* All other notifications are accepted and never answered. *)
          Continue
      | Some "initialize" ->
          respond_result id (initialize_result ());
          Continue
      | Some "ping" ->
          respond_result id (`Assoc []);
          Continue
      | Some "resources/list" ->
          respond_result id (resources_list pinned);
          Continue
      | Some "resources/read" -> (
          match read_resource pinned params with
          | Ok resource ->
              respond_result id (read_resource_result resource);
              Continue
          | Error message ->
              respond_error id (-32602) message;
              Continue)
      | Some "tools/list" ->
          respond_result id (tools_list ());
          Continue
      | Some "tools/call" -> (
          match call_tool pinned params with
          | Ok result ->
              respond_result id result;
              Continue
          | Error message ->
              respond_error id (-32602) message;
              Continue)
      | Some "shutdown" ->
          respond_result id (`Assoc []);
          Continue
      | Some other ->
          respond_error id (-32601)
            (Printf.sprintf "method '%s' is not supported" other);
          Continue)
  | _ ->
      respond_error None (-32600) "invalid JSON-RPC request";
      Continue

let serve pinned =
  let safe_handle json =
    try handle pinned json
    with exn ->
      if json_rpc_notification json then Continue
      else begin
        let id =
          match json with `Assoc entries -> List.assoc_opt "id" entries | _ -> None
        in
        respond_error id (-32603)
          (Printf.sprintf "internal error: %s" (Printexc.to_string exn));
        Continue
      end
  in
  let rec loop () =
    match input_line stdin with
    | exception End_of_file -> 0
    | line -> (
        match Yojson.Safe.from_string line with
        | exception Yojson.Json_error _ ->
            respond_error None (-32700) "malformed JSON";
            loop ()
        | `List _ ->
            respond_error None (-32600)
              "batch requests are not supported; send one JSON object per line";
            loop ()
        | json -> (
            match safe_handle json with
            | Stop -> 0
            | Continue -> loop ()))
  in
  loop ()

let run ?source ?repo ?role ?client ?project ?release () =
  try
    match select_pinned ~source ~repo ~role ~client ~project ~release with
    | Error message ->
        report message;
        2
    | Ok pinned -> serve pinned
  with
  | Sys_error message ->
      report message;
      2
  | Invalid_argument message ->
      report message;
      2
  | Unix.Unix_error (error, function_name, argument) ->
      report
        (Printf.sprintf "%s: %s %s" (Unix.error_message error) function_name
           argument);
      2
