open Nomia

module C = Nomia.Canonical
module Co = Nomia.Compose
module L = Nomia.Lock
module M = Nomia.Model
module R = Nomia.Release
module S = Nomia.Source

let today = "2026-11-23"
let compose_source = "fixtures/compose/source"
let compose_repos = "fixtures/compose/repos"

(* ------------------------------------------------------------------ *)
(* Helpers                                                            *)
(* ------------------------------------------------------------------ *)

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

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec copy_tree source destination =
  Unix.mkdir destination 0o755;
  Sys.readdir source
  |> Array.iter (fun name ->
         let source_path = Filename.concat source name in
         let destination_path = Filename.concat destination name in
         if Sys.is_directory source_path then
           copy_tree source_path destination_path
         else write_file destination_path (read_file source_path))

let rec remove_tree path =
  if Sys.is_directory path then begin
    Sys.readdir path
    |> Array.iter (fun name -> remove_tree (Filename.concat path name));
    Unix.rmdir path
  end
  else Sys.remove path

let with_temp_dir action =
  let temporary = Filename.temp_dir "nomia-compose-" "" in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree temporary with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () -> action temporary)

let with_copied_repo source action =
  with_temp_dir (fun temporary ->
      let destination = Filename.concat temporary "repo" in
      copy_tree source destination;
      action destination)

let load_source root =
  match S.load_and_validate ~root ~today with
  | Ok source -> source
  | Error diagnostics ->
      Alcotest.failf "load %s: %s" root
        (String.concat ", " (List.map (fun (d : M.diagnostic) -> d.code) diagnostics))

let resolve_ok ?repo ?project ?role ?client source =
  match Co.resolve ?repo ?project ?role ?client ~today source with
  | Ok policy_set -> policy_set
  | Error diagnostics ->
      Alcotest.failf "resolve: %s"
        (String.concat ", " (List.map (fun (d : M.diagnostic) -> d.code) diagnostics))

let resolve_error ?repo ?project ?role ?client source =
  match Co.resolve ?repo ?project ?role ?client ~today source with
  | Ok _ -> Alcotest.fail "resolve unexpectedly succeeded"
  | Error diagnostics -> diagnostics

let resolve_root_ok ?repo ?project ?role ?client root =
  match Co.resolve_root ?repo ?project ?role ?client ~today ~root () with
  | Ok policy_set -> policy_set
  | Error diagnostics ->
      Alcotest.failf "resolve_root: %s"
        (String.concat ", " (List.map (fun (d : M.diagnostic) -> d.code) diagnostics))

let codes diagnostics = List.map (fun (d : M.diagnostic) -> d.code) diagnostics

let has_code code diagnostics = List.mem code (codes diagnostics)

let has_severe_code code diagnostics =
  has_code code diagnostics
  &&
  List.exists
    (fun (d : M.diagnostic) ->
      d.code = code && contains_sub d.message "re-pin with an explicit reviewed change")
    diagnostics

let find_rule policy_set id =
  List.find_opt (fun (resolved : Co.resolved_rule) -> resolved.rule.id = id)
    policy_set.Co.rules

let require_rule policy_set id =
  match find_rule policy_set id with
  | Some resolved -> resolved
  | None -> Alcotest.failf "rule %s is missing from the PolicySet" id

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let jstring = function `String text -> text | _ -> ""

let jlist = function `List values -> values | _ -> []

let member_string key json =
  member key json |> Option.value ~default:`Null |> jstring

let member_list key json =
  member key json |> Option.value ~default:(`List []) |> jlist

let canonical_json value = C.to_string value

let has_absolute_path text =
  contains_sub text "/home/" || contains_sub text "/Users/"
  || contains_sub text "/private/" || contains_sub text "/tmp/"

let write_lock repo ?role ?client ?fragment ?digest
    ?(source_digest = "sha256:" ^ String.make 64 'a') ~release () =
  let lock =
    {
      L.schema = L.schema;
      release;
      digest;
      source_digest = Some source_digest;
      adapter_schema = 1;
      compiler = "agentctl/1";
      role;
      client;
      fragment;
      source = L.lock_file;
    }
  in
  write_file (Filename.concat repo L.lock_file) (L.to_yaml_string lock)

let release_digest ?(client = "generic-agents") ?role ?project
    ?(fragments = []) ~exceptions source =
  R.digest
    (R.build ~client ?role ?project ~fragments ~exceptions source)

let first_fragment policy_set =
  match policy_set.Co.fragments with
  | fragment :: _ -> fragment
  | [] -> Alcotest.fail "no repository fragment was selected"

(* ------------------------------------------------------------------ *)
(* Real corpus                                                        *)
(* ------------------------------------------------------------------ *)

let test_real_corpus_all_adapters () =
  let source = load_source ".." in
  let final_ids =
    source.rules
    |> List.filter (fun (rule : M.rule) -> rule.final)
    |> List.map (fun (rule : M.rule) -> rule.id)
  in
  List.iter
    (fun client ->
      let policy_set = resolve_ok ~client source in
      Alcotest.(check string) (client ^ " selects its adapter") client
        policy_set.adapter.id;
      Alcotest.(check int) (client ^ " keeps every organization rule")
        (List.length source.rules)
        (List.length policy_set.rules);
      List.iter
        (fun id ->
          let resolved = require_rule policy_set id in
          Alcotest.(check bool) (client ^ " keeps final rule " ^ id) true
            resolved.rule.final)
        final_ids;
      Alcotest.(check int) (client ^ " has no fragment") 0
        (List.length policy_set.fragments);
      let json = canonical_json (Co.explain_json policy_set) in
      Alcotest.(check bool) (client ^ " explain has no absolute path") false
        (has_absolute_path json);
      Alcotest.(check bool) (client ^ " explain names the schema") true
        (contains_sub json "nomia-explain/v2"))
    [ "generic-agents"; "opencode"; "erde"; "saguaro" ]

let test_optional_roles () =
  let source = load_source ".." in
  let plain = resolve_ok source in
  let implementer = resolve_ok ~role:"implementer" source in
  let reviewer = resolve_ok ~role:"code-reviewer" source in
  Alcotest.(check bool) "no role selected by default" true (plain.role = None);
  Alcotest.(check string) "implementer selected" "implementer"
    (match implementer.role with Some role -> role.name | None -> "");
  Alcotest.(check string) "reviewer selected" "code-reviewer"
    (match reviewer.role with Some role -> role.name | None -> "");
  Alcotest.(check string) "implementer charter text is stable"
    (match
       List.find_opt
         (fun (role : M.role) -> role.name = "implementer")
         source.roles
     with
    | Some role -> role.charter
    | None -> "")
    (match implementer.role with Some role -> role.charter | None -> "");
  Alcotest.(check bool) "role selection changes the digest" true
    (plain.digest <> implementer.digest && implementer.digest <> reviewer.digest);
  let implementer_again = resolve_ok ~role:"implementer" source in
  Alcotest.(check string) "same role selection is byte-stable" implementer.digest
    implementer_again.digest;
  Alcotest.(check string) "same role explain is byte-stable"
    (canonical_json (Co.explain_json implementer))
    (canonical_json (Co.explain_json implementer_again));
  Alcotest.(check string) "explain records the role" "code-reviewer"
    (member_string "role" (Co.explain_json reviewer))

(* ------------------------------------------------------------------ *)
(* Repository fragments                                               *)
(* ------------------------------------------------------------------ *)

let test_fragment_adds_rule () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "fragment-add") source
  in
  let fragment = first_fragment policy_set in
  Alcotest.(check string) "fragment path is repository-relative" "AGENTS.md"
    fragment.path;
  Alcotest.(check bool) "fragment digest is a sha256" true
    (String.length fragment.digest = 71 && contains_sub fragment.digest "sha256:");
  Alcotest.(check bool) "fragment prose is preserved" true
    (contains_sub fragment.prose "Use repository tabs");
  let addition = require_rule policy_set "repo.style" in
  Alcotest.(check bool) "fragment rule origin is repository" true
    (addition.origin = Co.Repository);
  Alcotest.(check bool) "fragment rule is not final" false addition.rule.final;
  Alcotest.(check int) "fragment adds one rule" 7 (List.length policy_set.rules);
  Alcotest.(check int) "fragment digest changes the release digest" 71
    (String.length policy_set.digest);
  Alcotest.(check bool) "final organization rule survives" true
    (require_rule policy_set "org.final").rule.final

let test_fragment_conflicts () =
  let source = load_source compose_source in
  let expect repo code =
    let diagnostics =
      resolve_error ~repo:(Filename.concat compose_repos repo) source
    in
    Alcotest.(check bool) (repo ^ " reports " ^ code) true
      (has_code code diagnostics)
  in
  expect "final-key" "compose.rule.final-key";
  expect "relax-final" "compose.override.final";
  expect "duplicate-id" "compose.rule.duplicate-id";
  expect "key-conflict" "compose.rule.key-conflict";
  expect "unknown-supersedes" "compose.rule.supersedes";
  expect "relax-unknown" "compose.override.unknown"

let test_non_final_relax () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "relax-open") source
  in
  Alcotest.(check bool) "a non-final organization rule may be relaxed" true
    (find_rule policy_set "org.open" = None);
  Alcotest.(check bool) "final rules still survive a repository relax" true
    (find_rule policy_set "org.final" <> None);
  Alcotest.(check bool) "the relax is explained" true
    (List.exists
       (fun item ->
         member_string "kind" item = "relax"
         && member_string "target" item = "org.open")
       (member_list "overrides" (Co.explain_json policy_set)));
  Alcotest.(check bool) "the relax is visible as a warning" true
    (List.exists
       (fun warning -> contains_sub warning "relaxes organization rule")
       policy_set.warnings)

let test_superseding_revision () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "supersede") source
  in
  let open_rule = require_rule policy_set "org.open" in
  Alcotest.(check int) "repository revision replaces the organization revision" 2
    open_rule.rule.revision;
  Alcotest.(check bool) "superseding rule originates at the repository" true
    (open_rule.origin = Co.Repository);
  Alcotest.(check (option string)) "superseding value is effective"
    (Some "repository") open_rule.rule.value;
  Alcotest.(check bool) "override is explained" true
    (List.exists
       (fun item ->
         member_string "kind" item = "supersede"
         && member_string "target" item = "org.open@1")
       (member_list "overrides" (Co.explain_json policy_set)));
  Alcotest.(check bool) "override is visible as a warning" true
    (List.exists
       (fun warning -> contains_sub warning "supersedes organization rule")
       policy_set.warnings)

let test_final_supersede_is_ignored () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "supersede-final") source
  in
  let final_rule = require_rule policy_set "org.final" in
  Alcotest.(check bool) "final organization rule keeps organization origin" true
    (final_rule.origin = Co.Organization);
  Alcotest.(check bool) "final organization rule stays effective" true
    final_rule.rule.final;
  Alcotest.(check int) "final organization rule keeps its revision" 1
    final_rule.rule.revision;
  Alcotest.(check bool) "final supersede is called out" true
    (List.exists
       (fun warning -> contains_sub warning "stays effective")
       policy_set.warnings)

(* ------------------------------------------------------------------ *)
(* Exceptions                                                         *)
(* ------------------------------------------------------------------ *)

let test_active_repository_exceptions () =
  let source = load_source compose_source in
  let widget =
    resolve_ok ~repo:(Filename.concat compose_repos "widget") source
  in
  let ids =
    List.map (fun (exception_ : M.exception_) -> exception_.id) widget.exceptions
  in
  Alcotest.(check (list string)) "widget activates repo and path exceptions"
    [ "exc.path-gate"; "exc.repo-open" ] ids;
  let open_rule = require_rule widget "org.open" in
  Alcotest.(check (option string)) "repo exception is attached to its rule"
    (Some "exc.repo-open")
    (Option.map (fun (exception_ : M.exception_) -> exception_.id)
       open_rule.active_exception);
  let gate_rule = require_rule widget "org.gate" in
  Alcotest.(check (option string)) "path exception is attached to its rule"
    (Some "exc.path-gate")
    (Option.map (fun (exception_ : M.exception_) -> exception_.id)
       gate_rule.active_exception);
  Alcotest.(check bool) "task exception is never active for render" false
    (List.mem "exc.task-review" ids);
  Alcotest.(check (list string)) "explain records active exception ids" ids
    (member_list "exceptions" (Co.explain_json widget) |> List.map jstring);
  Alcotest.(check (list string)) "release manifest records active exception ids"
    ids
    (member_list "exceptions" (R.manifest_json widget.release)
    |> List.map jstring)

let test_out_of_scope_exceptions () =
  let source = load_source compose_source in
  let other =
    resolve_ok ~repo:(Filename.concat compose_repos "other") source
  in
  Alcotest.(check (list string)) "other repository activates no exception"
    []
    (List.map (fun (exception_ : M.exception_) -> exception_.id) other.exceptions);
  Alcotest.(check bool) "repo exception is not active elsewhere" true
    ((require_rule other "org.open").active_exception = None);
  let no_repo = resolve_ok source in
  Alcotest.(check int) "no repository context activates no exception" 0
    (List.length no_repo.exceptions)

let test_expired_exception_rejected_by_validation () =
  match
    S.load_and_validate ~root:"fixtures/invalid/expired-exception"
      ~today:"2027-02-01"
  with
  | Ok _ -> Alcotest.fail "expired exception passed source validation"
  | Error diagnostics ->
      Alcotest.(check bool) "expired exception names exception.expires" true
        (has_code "exception.expires" diagnostics)

(* ------------------------------------------------------------------ *)
(* Fragment discovery                                                 *)
(* ------------------------------------------------------------------ *)

let test_generated_fragment_is_skipped () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "generated") source
  in
  Alcotest.(check bool) "generated AGENTS.md is not a fragment" true
    (policy_set.fragments = []);
  Alcotest.(check bool) "skipping a generated fragment warns" true
    (List.exists
       (fun warning -> contains_sub warning "generated")
       policy_set.warnings)

let test_fragment_precedence () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "both") source
  in
  let fragment = first_fragment policy_set in
  Alcotest.(check string) "AGENTS.fragment.md is preferred over AGENTS.md"
    "AGENTS.fragment.md" fragment.path;
  Alcotest.(check bool) "preferred fragment prose is used" true
    (contains_sub fragment.prose "dedicated fragment is preferred");
  Alcotest.(check bool) "preferred fragment rule is effective" true
    (find_rule policy_set "repo.fragment-preferred" <> None);
  Alcotest.(check bool) "the other AGENTS.md prose is not selected" false
    (contains_sub fragment.prose "Wrong choice")

let test_hand_authored_fragment_is_used () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "widget") source
  in
  Alcotest.(check bool) "hand-authored AGENTS.md is used" true
    ((first_fragment policy_set).path = "AGENTS.md");
  Alcotest.(check bool) "hand-authored prose survives" true
    (contains_sub (first_fragment policy_set).prose "tab indentation");
  Alcotest.(check bool) "hand-authored fragment is not skipped" false
    (List.exists
       (fun warning -> contains_sub warning "skipped as a fragment")
       policy_set.warnings)

(* ------------------------------------------------------------------ *)
(* Monorepo project composition                                       *)
(* ------------------------------------------------------------------ *)

let project_fragment id body =
  Printf.sprintf
    "---\n\
     rules:\n\
     \  - id: %s\n\
     \    revision: 1\n\
     \    owner: wyo-engineering\n\
     \    strength: must\n\
     \    scope: [code-change]\n\
     \    final: false\n\
     \    kind: instruction\n\
     ---\n\
     %s\n"
    id body

let with_project_repo action =
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "repo" in
      let tuna = Filename.concat repo "tuna" in
      let pp = Filename.concat repo "pp" in
      Unix.mkdir repo 0o755;
      Unix.mkdir tuna 0o755;
      Unix.mkdir pp 0o755;
      write_file (Filename.concat repo "AGENTS.fragment.md")
        "Wyo-wide repository instructions.\n";
      write_file
        (Filename.concat tuna "AGENTS.fragment.md")
        (project_fragment "repo.tuna-stack"
           "Tuna uses the tree-calculus verification path.");
      write_file
        (Filename.concat pp "AGENTS.fragment.md")
        (project_fragment "repo.pp-stack"
           "Pricklypear uses the habitat verification path.");
      action repo)

let test_project_composition () =
  let source = load_source compose_source in
  with_project_repo (fun repo ->
      let root = resolve_ok ~repo source in
      let tuna = resolve_ok ~repo ~project:"tuna" source in
      let pp = resolve_ok ~repo ~project:"pp" source in
      Alcotest.(check (option string)) "root has no project" None root.project;
      Alcotest.(check (option string)) "tuna project is recorded" (Some "tuna")
        tuna.project;
      Alcotest.(check int) "root selects one fragment" 1
        (List.length root.fragments);
      Alcotest.(check int) "project adds one fragment" 2
        (List.length tuna.fragments);
      Alcotest.(check (list string)) "root then project order"
        [ "AGENTS.fragment.md"; "tuna/AGENTS.fragment.md" ]
        (List.map
           (fun (fragment : Co.fragment) -> fragment.path)
           tuna.fragments);
      Alcotest.(check bool) "root fragment has root scope" true
        ((List.hd tuna.fragments).Co.scope = Co.Root);
      Alcotest.(check bool) "project fragment has project scope" true
        ((List.nth tuna.fragments 1).Co.scope = Co.Project);
      Alcotest.(check bool) "tuna rule is effective" true
        (find_rule tuna "repo.tuna-stack" <> None);
      Alcotest.(check bool) "sibling pp rule is excluded" true
        (find_rule tuna "repo.pp-stack" = None);
      Alcotest.(check bool) "pp rule is effective for pp" true
        (find_rule pp "repo.pp-stack" <> None);
      Alcotest.(check bool) "project selection changes the digest" true
        (root.digest <> tuna.digest && tuna.digest <> pp.digest);
      let explain = Co.explain_json tuna in
      Alcotest.(check string) "explain records the project" "tuna"
        (member_string "project" explain);
      Alcotest.(check int) "explain records the fragment chain" 2
        (List.length (member_list "fragments" explain)))

let test_project_exception_scope () =
  let source = load_source compose_source in
  let project_exception =
    {
      M.id = "exc.project-open";
      rule = "org.open";
      scope = "project:tuna";
      reason = "Tuna has a reviewed project-local exception.";
      approver = "wyo-engineering";
      expires = "2030-01-01";
      source = "exceptions.yaml";
      line = 1;
    }
  in
  let source =
    { source with exceptions = source.exceptions @ [ project_exception ] }
  in
  with_project_repo (fun repo ->
      let tuna = resolve_ok ~repo ~project:"tuna" source in
      let pp = resolve_ok ~repo ~project:"pp" source in
      Alcotest.(check bool) "project exception is active in its project" true
        (List.exists
           (fun (exception_ : M.exception_) ->
             exception_.id = "exc.project-open")
           tuna.exceptions);
      Alcotest.(check bool) "project exception stays out of siblings" false
        (List.exists
           (fun (exception_ : M.exception_) ->
             exception_.id = "exc.project-open")
           pp.exceptions))

let test_project_validation () =
  let source = load_source compose_source in
  with_project_repo (fun repo ->
      let missing =
        resolve_error ~repo ~project:"missing" source
      in
      Alcotest.(check bool) "missing project is diagnosed" true
        (has_code "compose.project.missing" missing);
      let unsafe = resolve_error ~repo ~project:"../escape" source in
      Alcotest.(check bool) "escaping project is diagnosed" true
        (has_code "compose.project.path" unsafe);
      let no_repo = resolve_error ~project:"tuna" source in
      Alcotest.(check bool) "project requires a repository" true
        (has_code "compose.project.repo" no_repo))

(* ------------------------------------------------------------------ *)
(* Unknown selections                                                 *)
(* ------------------------------------------------------------------ *)

let test_unknown_client_and_role () =
  let source = load_source compose_source in
  let client_diagnostics = resolve_error ~client:"not-a-client" source in
  Alcotest.(check bool) "unknown client is diagnosed" true
    (has_code "compose.client.unknown" client_diagnostics);
  let role_diagnostics = resolve_error ~role:"not-a-role" source in
  Alcotest.(check bool) "unknown role is diagnosed" true
    (has_code "compose.role.unknown" role_diagnostics)

(* ------------------------------------------------------------------ *)
(* Locks                                                              *)
(* ------------------------------------------------------------------ *)

let test_lock_parse_and_find () =
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "repo" in
      Unix.mkdir repo 0o755;
      Alcotest.(check bool) "no lock means None" true
        (match L.find_in_repo ~repo with Ok None -> true | _ -> false);
      write_lock repo ~release:"0.1.0" ~digest:("sha256:" ^ String.make 64 'a')
        ~role:"implementer" ~client:"opencode" ~fragment:"AGENTS.md" ();
      match L.find_in_repo ~repo with
      | Error _ -> Alcotest.fail "written lock did not parse"
      | Ok None -> Alcotest.fail "written lock was not found"
      | Ok (Some lock) ->
          Alcotest.(check string) "lock schema round-trips" L.schema lock.schema;
          Alcotest.(check string) "lock release round-trips" "0.1.0"
            lock.release;
          Alcotest.(check int) "lock adapter schema round-trips" 1
            lock.adapter_schema;
          Alcotest.(check string) "lock compiler round-trips" "agentctl/1"
            lock.compiler;
          Alcotest.(check (option string)) "lock role round-trips"
            (Some "implementer") lock.role;
          Alcotest.(check (option string)) "lock client round-trips"
            (Some "opencode") lock.client;
          Alcotest.(check (option string)) "lock fragment round-trips"
            (Some "AGENTS.md") lock.fragment);
  with_temp_dir (fun temporary ->
      let path = Filename.concat temporary "nomia.lock" in
      write_file path
        "schema: nomia-lock/v2\nsource_digest: sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\nunknown: value\n";
      match L.parse ~path with
      | Ok _ -> Alcotest.fail "unknown lock field was accepted"
      | Error diagnostics ->
          Alcotest.(check bool) "unknown lock field is diagnosed" true
            (has_code "lock.unknown-field" diagnostics))

let test_matching_lock_selects_role_and_client () =
  let source = load_source ".." in
  let digest =
    release_digest ~client:"opencode" ~role:"implementer" ~exceptions:[] source
  in
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "repo" in
      Unix.mkdir repo 0o755;
      write_lock repo ~release:source.policy.release ~digest
        ~source_digest:(R.source_digest source) ~role:"implementer"
        ~client:"opencode" ();
      let policy_set = resolve_ok ~repo source in
      Alcotest.(check string) "lock role is selected" "implementer"
        (match policy_set.role with Some role -> role.name | None -> "");
      Alcotest.(check string) "lock client selects the adapter" "opencode"
        policy_set.adapter.id;
      Alcotest.(check bool) "lock is recorded" true (policy_set.lock <> None);
      Alcotest.(check string) "lock digest matches the resolved digest" digest
        policy_set.digest;
      let explain = Co.explain_json policy_set in
      Alcotest.(check string) "explain records the lock role" "implementer"
        (match member "lock" explain with
        | Some lock -> member_string "role" lock
        | None -> "");
      (match Co.resolve ~repo ~client:"erde" source with
      | Ok _ ->
          Alcotest.fail "a client override silently reused another client's pin"
      | Error diagnostics ->
          Alcotest.(check bool) "client override requires a reviewed re-pin"
            true (has_code "compose.lock.digest" diagnostics)))

let test_cli_role_overrides_lock () =
  let source = load_source ".." in
  let lock_digest =
    release_digest ~client:"generic-agents" ~role:"implementer" ~exceptions:[]
      source
  in
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "repo" in
      Unix.mkdir repo 0o755;
      write_lock repo ~release:source.policy.release ~digest:lock_digest
        ~source_digest:(R.source_digest source) ~role:"implementer"
        ~client:"generic-agents" ();
      Alcotest.(check string) "the matching lock resolves"
        lock_digest
        (resolve_ok ~repo source).digest;
      match Co.resolve ~repo ~role:"code-reviewer" source with
      | Ok _ ->
          Alcotest.fail "a role override silently reused another role's pin"
      | Error diagnostics ->
          Alcotest.(check bool) "role override requires a reviewed re-pin" true
            (has_code "compose.lock.digest" diagnostics))

let test_lock_fragment_selection () =
  let source = load_source compose_source in
  let fixture_repo = Filename.concat compose_repos "lock-fragment" in
  let fragment_digest =
    "sha256:" ^ C.sha256_hex (read_file (Filename.concat fixture_repo "docs/repo.md"))
  in
  let digest =
    release_digest ~fragments:[ ("docs/repo.md", fragment_digest) ]
      ~exceptions:[] source
  in
  with_copied_repo fixture_repo (fun repo ->
      write_lock repo ~release:source.policy.release ~digest
        ~source_digest:(R.source_digest source)
        ~fragment:"docs/repo.md" ();
      let policy_set = resolve_ok ~repo source in
      let fragment = first_fragment policy_set in
      Alcotest.(check string) "locked fragment path stays repository-relative"
        "docs/repo.md" fragment.path;
      Alcotest.(check bool) "lock fragment outranks AGENTS.md" true
        (contains_sub fragment.prose "selected by the lock"
        && not (contains_sub fragment.prose "Not the locked fragment"));
      Alcotest.(check string) "locked fragment digest is recorded"
        fragment_digest fragment.digest;
      Alcotest.(check bool) "locked fragment rules are effective" true
        (find_rule policy_set "repo.locked" <> None);
      Alcotest.(check string) "locked fragment digest participates in release"
        digest policy_set.digest)

let test_lock_mismatch () =
  let source = load_source ".." in
  let actual = release_digest ~exceptions:[] source in
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "digest" in
      Unix.mkdir repo 0o755;
      write_lock repo ~release:source.policy.release
        ~source_digest:(R.source_digest source)
        ~digest:("sha256:" ^ String.make 64 '0') ();
      let diagnostics = resolve_error ~repo source in
      Alcotest.(check bool) "digest mismatch is diagnosed" true
        (has_code "compose.lock.digest" diagnostics);
      Alcotest.(check bool) "digest mismatch asks for a reviewed re-pin" true
        (has_severe_code "compose.lock.digest" diagnostics));
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "release" in
      Unix.mkdir repo 0o755;
      write_lock repo ~release:"9.9.9" ~digest:actual
        ~source_digest:(R.source_digest source) ();
      let diagnostics = resolve_error ~repo source in
      Alcotest.(check bool) "release mismatch is diagnosed" true
        (has_code "compose.lock.release" diagnostics);
      Alcotest.(check bool) "release mismatch asks for a reviewed re-pin" true
        (has_severe_code "compose.lock.release" diagnostics));
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "fragment" in
      Unix.mkdir repo 0o755;
      write_lock repo ~release:source.policy.release ~digest:actual
        ~source_digest:(R.source_digest source)
        ~fragment:"missing.fragment.md" ();
      let diagnostics = resolve_error ~repo source in
      Alcotest.(check bool) "missing locked fragment is diagnosed" true
        (has_code "compose.fragment.missing" diagnostics))

(* ------------------------------------------------------------------ *)
(* Capability mapping                                                 *)
(* ------------------------------------------------------------------ *)

let support_of policy_set id = (require_rule policy_set id).support

let test_capability_mapping () =
  let source = load_source ".." in
  let generic = resolve_ok ~client:"generic-agents" source in
  let opencode = resolve_ok ~client:"opencode" source in
  let erde = resolve_ok ~client:"erde" source in
  let saguaro = resolve_ok ~client:"saguaro" source in
  List.iter
    (fun (policy_set : Co.t) ->
      Alcotest.(check bool)
        (policy_set.adapter.id ^ " supports ci-verification") true
        (support_of policy_set "engineering.verify-change" = `Supported);
      Alcotest.(check bool)
        (policy_set.adapter.id ^ " leaves advisory claims supported") true
        (support_of policy_set "security.untrusted-input" = `Supported);
      let expected =
        if policy_set.adapter.id = "opencode" then [ "host-hook" ]
        else [ "host-hook"; "permissions" ]
      in
      Alcotest.(check (list string))
        (policy_set.adapter.id ^ " names the unsupported consent controls")
        expected
        (require_rule policy_set "operations.authority").unsupported_controls)
    [ generic; opencode; erde; saguaro ];
  List.iter
    (fun (policy_set : Co.t) ->
      Alcotest.(check bool)
        (policy_set.adapter.id ^ " cannot enforce consent without hooks") true
        (support_of policy_set "operations.authority" = `Unsupported);
      Alcotest.(check bool)
        (policy_set.adapter.id ^ " warns about the missing host hook") true
        (List.exists
           (fun warning -> contains_sub warning "host hooks")
           policy_set.warnings))
    [ generic; erde; saguaro ];
  Alcotest.(check bool) "opencode maps native permission ask as partial consent"
    true
    (support_of opencode "operations.authority" = `Partial);
  Alcotest.(check bool) "generic cannot enforce filesystem or permissions" true
    (support_of generic "security.credentials" = `Unsupported);
  Alcotest.(check (list string)) "generic names both unsupported controls"
    [ "filesystem"; "permissions" ]
    (require_rule generic "security.credentials").unsupported_controls;
  Alcotest.(check bool) "opencode partially maps the prohibition" true
    (support_of opencode "security.credentials" = `Partial);
  Alcotest.(check (list string)) "opencode names the unsupported filesystem control"
    [ "filesystem" ]
    (require_rule opencode "security.credentials").unsupported_controls;
  Alcotest.(check bool) "erde partially maps the prohibition" true
    (support_of erde "security.credentials" = `Partial);
  Alcotest.(check bool) "saguaro cannot enforce the prohibition" true
    (support_of saguaro "security.credentials" = `Unsupported);
  Alcotest.(check bool) "no adapter provides ci-review" true
    (support_of generic "engineering.independent-review" = `Unsupported
    && support_of opencode "engineering.independent-review" = `Unsupported
    && support_of erde "engineering.independent-review" = `Unsupported
    && support_of saguaro "engineering.independent-review" = `Unsupported);
  Alcotest.(check bool) "unsupported controls appear as warnings" true
    (List.exists
       (fun warning ->
         contains_sub warning
           "control 'filesystem' is unsupported by adapter 'generic-agents'")
       generic.warnings);
  Alcotest.(check bool) "advisory instructions are never promoted" true
    (match require_rule generic "security.untrusted-input" with
    | resolved -> resolved.rule.enforcement <> None
                  && resolved.unsupported_controls = [])

(* ------------------------------------------------------------------ *)
(* Explain and determinism                                            *)
(* ------------------------------------------------------------------ *)

let test_explain_shape () =
  let source = load_source compose_source in
  let policy_set =
    resolve_ok ~repo:(Filename.concat compose_repos "widget")
      ~role:"implementer" source
  in
  let explain = Co.explain_json policy_set in
  Alcotest.(check string) "explain schema" "nomia-explain/v2"
    (member_string "schema" explain);
  Alcotest.(check string) "explain release version" source.policy.release
    (match member "release" explain with
    | Some release -> member_string "version" release
    | None -> "");
  Alcotest.(check string) "explain release digest" policy_set.digest
    (match member "release" explain with
    | Some release -> member_string "digest" release
    | None -> "");
  Alcotest.(check string) "explain source digest" policy_set.source_digest
    (member_string "source_digest" explain);
  Alcotest.(check string) "explain adapter" "generic-agents"
    (match member "adapter" explain with
    | Some adapter -> member_string "id" adapter
    | None -> "");
  Alcotest.(check string) "explain role" "implementer"
    (member_string "role" explain);
  Alcotest.(check string) "explain fragment path" "AGENTS.md"
    (match member "fragments" explain with
    | Some (`List (fragment :: _)) -> member_string "path" fragment
    | _ -> "");
  (* The explain document is artifact-stable: expiry reasoning uses [today],
     but the date is not embedded, so renders on different days stay equal. *)
  Alcotest.(check bool) "explain has no today field" true
    (match explain with `Assoc fields -> not (List.mem_assoc "today" fields) | _ -> false);
  Alcotest.(check int) "explain lists every effective rule"
    (List.length policy_set.rules)
    (List.length (member_list "rules" explain));
  let rule = member_list "rules" explain |> List.hd in
  List.iter
    (fun key ->
      Alcotest.(check bool) ("explain rule has " ^ key) true
        (member key rule <> None))
    [
      "id";
      "revision";
      "strength";
      "final";
      "kind";
      "layer";
      "origin";
      "source";
      "line";
      "enforcement";
      "actions";
      "guards";
      "exception";
      "support";
      "unsupported_controls";
    ];
  Alcotest.(check bool) "explain carries a capability matrix" true
    (match member "capabilities" explain with
    | Some (`Assoc _) -> true
    | _ -> false);
  Alcotest.(check bool) "explain warnings are a list" true
    (match member "warnings" explain with Some (`List _) -> true | _ -> false);
  let text = Co.explain_text policy_set in
  Alcotest.(check bool) "text explain names itself" true
    (contains_sub text "nomia explain");
  Alcotest.(check bool) "text explain names a rule" true
    (contains_sub text "org.final");
  Alcotest.(check bool) "text explain carries support" true
    (contains_sub text "supported")

let test_determinism_and_no_path_leak () =
  let source = load_source ".." in
  let first = resolve_ok ~role:"implementer" source in
  let second = resolve_ok ~role:"implementer" source in
  Alcotest.(check string) "two resolves produce identical digests" first.digest
    second.digest;
  Alcotest.(check string) "two resolves produce identical explain bytes"
    (canonical_json (Co.explain_json first))
    (canonical_json (Co.explain_json second));
  Alcotest.(check string) "two resolves produce identical explain text"
    (Co.explain_text first) (Co.explain_text second);
  let other_day =
    match Co.resolve ~role:"implementer" ~today:"2030-01-01" source with
    | Ok policy_set -> policy_set
    | Error _ -> Alcotest.fail "resolve with another today failed"
  in
  Alcotest.(check string) "today does not enter the release digest" first.digest
    other_day.digest;
  Alcotest.(check string) "today is recorded for expiry reasoning"
    "2030-01-01" other_day.today;
  let json = canonical_json (Co.explain_json first) in
  Alcotest.(check bool) "explain JSON does not leak a private path" false
    (has_absolute_path json);
  Alcotest.(check bool) "explain text does not leak a private path" false
    (has_absolute_path (Co.explain_text first));
  Alcotest.(check bool) "explain does not leak the source root" false
    (contains_sub json source.root);
  Alcotest.(check bool) "explain has no secret shapes" false
    (contains_sub json "AKIA"
    || contains_sub json "BEGIN PRIVATE KEY"
    || contains_sub json "password=");
  let recorded_paths =
    (member_list "rules" (Co.explain_json first)
    |> List.map (fun rule -> member_string "source" rule))
    @ (match member "fragment" (Co.explain_json first) with
      | Some (`Assoc _ as fragment) -> [ member_string "path" fragment ]
      | _ -> [])
  in
  Alcotest.(check bool) "every recorded explain path is relative" true
    (List.for_all (fun path -> path = "" || path.[0] <> '/') recorded_paths)

let test_absolute_repository_location_does_not_change_explain () =
  let source = load_source compose_source in
  with_temp_dir (fun first_parent ->
      with_temp_dir (fun second_parent ->
          let first_repo = Filename.concat first_parent "widget" in
          let second_repo = Filename.concat second_parent "widget" in
          copy_tree (Filename.concat compose_repos "widget") first_repo;
          copy_tree (Filename.concat compose_repos "widget") second_repo;
          let first = resolve_ok ~repo:first_repo source in
          let second = resolve_ok ~repo:second_repo source in
          Alcotest.(check string)
            "same repository content at different absolute paths explains identically"
            (canonical_json (Co.explain_json first))
            (canonical_json (Co.explain_json second));
          Alcotest.(check bool) "neither explain leaks an absolute path" false
            (has_absolute_path (canonical_json (Co.explain_json first))
            || has_absolute_path (canonical_json (Co.explain_json second)))))

let test_resolve_root () =
  let policy_set = resolve_root_ok ".." in
  Alcotest.(check int) "resolve_root loads the real corpus" 10
    (List.length policy_set.rules);
  Alcotest.(check string) "resolve_root records today" today policy_set.today;
  Alcotest.(check bool) "resolve_root sets a release digest" true
    (String.length policy_set.digest = 71)


let test_invalid_fragment_is_rejected () =
  let source = load_source ".." in
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "bad-repo" in
      Unix.mkdir repo 0o755;
      let fragment = Filename.concat repo "AGENTS.fragment.md" in
      let body =
        "---\n\
         rules:\n\
         \  - id: repo.bad\n\
         \    revision: 1\n\
         \    owner: TODO\n\
         \    strength: must\n\
         \    scope: [repository]\n\
         \    final: false\n\
         \    kind: instruction\n\
         \    actions: [totally.unknown]\n\
         \    enforcement:\n\
         \      class: prohibition\n\
         \      controls: []\n\
         ---\n\
         Prose with AKIAIOSFODNN7EXAMPLE and /home/user/.ssh/id_rsa.\n"
      in
      Out_channel.with_open_bin fragment (fun channel ->
          output_string channel body);
      match Co.resolve ~repo source with
      | Ok _ -> Alcotest.fail "an invalid repository fragment was accepted"
      | Error diagnostics ->
          let codes = List.map (fun (d : M.diagnostic) -> d.code) diagnostics in
          List.iter
            (fun code ->
              Alcotest.(check bool) ("fragment diagnostic: " ^ code) true
                (List.mem code codes))
            [
              "rule.owner.placeholder";
              "rule.action.unknown";
              "rule.enforcement.controls";
              "fragment.private-path";
              "fragment.secret";
            ])


let test_fragment_rule_budget_is_org_only () =
  let source = load_source ".." in
  with_temp_dir (fun temporary ->
      let repo = Filename.concat temporary "budget-repo" in
      Unix.mkdir repo 0o755;
      let buffer = Buffer.create 1024 in
      Buffer.add_string buffer "---\nrules:\n";
      for index = 1 to 7 do
        Buffer.add_string buffer
          (Printf.sprintf
             "  - id: repo.r%d\n    revision: 1\n    owner: wyo-engineering\n    strength: must\n    scope: [repository]\n    final: false\n    kind: instruction\n    actions: [repo.read]\n"
             index)
      done;
      Buffer.add_string buffer "---\nRepository rules stay outside the organization core budget.\n";
      Out_channel.with_open_bin (Filename.concat repo "AGENTS.fragment.md")
        (fun channel -> output_string channel (Buffer.contents buffer));
      match Co.resolve ~repo source with
      | Error diagnostics ->
          Alcotest.failf "valid fragment rejected: %s"
            (String.concat "; "
               (List.map (fun (d : M.diagnostic) -> d.code) diagnostics))
      | Ok policy_set ->
          let repository_rules =
            List.filter
              (fun (resolved : Co.resolved_rule) ->
                resolved.origin = Co.Repository)
              policy_set.rules
          in
          Alcotest.(check int) "seven repository rules compose" 7
            (List.length repository_rules))

(* ------------------------------------------------------------------ *)
(* Suite                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia_compose"
    [
      ( "real corpus",
        [
          Alcotest.test_case "all adapters" `Quick
            test_real_corpus_all_adapters;
          Alcotest.test_case "optional roles" `Quick test_optional_roles;
        ] );
      ( "fragments",
        [
          Alcotest.test_case "adds a rule" `Quick test_fragment_adds_rule;
          Alcotest.test_case "conflicts" `Quick test_fragment_conflicts;
          Alcotest.test_case "non-final relax" `Quick test_non_final_relax;
          Alcotest.test_case "superseding revision" `Quick
            test_superseding_revision;
          Alcotest.test_case "final supersede ignored" `Quick
            test_final_supersede_is_ignored;
          Alcotest.test_case "generated fragment skipped" `Quick
            test_generated_fragment_is_skipped;
          Alcotest.test_case "hand-authored fragment used" `Quick
            test_hand_authored_fragment_is_used;
          Alcotest.test_case "fragment precedence" `Quick
            test_fragment_precedence;
          Alcotest.test_case "invalid fragment rejected" `Quick
            test_invalid_fragment_is_rejected;
          Alcotest.test_case "fragment rules do not consume the org budget" `Quick
            test_fragment_rule_budget_is_org_only;
        ] );
      ( "projects",
        [
          Alcotest.test_case "monorepo project composition" `Quick
            test_project_composition;
          Alcotest.test_case "project exception scope" `Quick
            test_project_exception_scope;
          Alcotest.test_case "project validation" `Quick
            test_project_validation;
        ] );
      ( "exceptions",
        [
          Alcotest.test_case "active repository exceptions" `Quick
            test_active_repository_exceptions;
          Alcotest.test_case "out of scope exceptions" `Quick
            test_out_of_scope_exceptions;
          Alcotest.test_case "expired rejected by validation" `Quick
            test_expired_exception_rejected_by_validation;
        ] );
      ( "selection",
        [
          Alcotest.test_case "unknown client and role" `Quick
            test_unknown_client_and_role;
          Alcotest.test_case "resolve_root" `Quick test_resolve_root;
        ] );
      ( "locks",
        [
          Alcotest.test_case "parse and find" `Quick test_lock_parse_and_find;
          Alcotest.test_case "matching lock" `Quick
            test_matching_lock_selects_role_and_client;
          Alcotest.test_case "CLI role override" `Quick
            test_cli_role_overrides_lock;
          Alcotest.test_case "locked fragment" `Quick
            test_lock_fragment_selection;
          Alcotest.test_case "mismatch" `Quick test_lock_mismatch;
        ] );
      ( "capabilities",
        [ Alcotest.test_case "classification and warnings" `Quick
            test_capability_mapping ] );
      ( "explain",
        [
          Alcotest.test_case "shape" `Quick test_explain_shape;
          Alcotest.test_case "determinism and no path leak" `Quick
            test_determinism_and_no_path_leak;
          Alcotest.test_case "path-independent explain" `Quick
            test_absolute_repository_location_does_not_change_explain;
        ] );
    ]
