open Nomia

module C = Nomia.Canonical
module Co = Nomia.Compose
module M = Nomia.Model
module R = Nomia.Release
module Rd = Nomia.Render
module S = Nomia.Source

let real_root = ".."
let today = "2030-01-01"

(* ------------------------------------------------------------------ *)
(* Helpers                                                            *)
(* ------------------------------------------------------------------ *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec remove_tree path =
  if Sys.is_directory path then begin
    Sys.readdir path
    |> Array.iter (fun name -> remove_tree (Filename.concat path name));
    Unix.rmdir path
  end
  else Sys.remove path

let with_temp_dir action =
  let temporary = Filename.temp_dir "nomia-render-" "" in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree temporary with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () -> action temporary)

let load_source () =
  match S.load_and_validate ~root:real_root ~today with
  | Ok source -> source
  | Error diagnostics ->
      Alcotest.failf "load %s: %s" real_root
        (String.concat ", "
           (List.map (fun (diagnostic : M.diagnostic) -> diagnostic.code)
              diagnostics))

let resolve_ok ?repo ?project ?role ?client source =
  match Co.resolve ?repo ?project ?role ?client ~today source with
  | Ok policy_set -> policy_set
  | Error diagnostics ->
      Alcotest.failf "resolve: %s"
        (String.concat ", "
           (List.map (fun (diagnostic : M.diagnostic) -> diagnostic.code)
              diagnostics))

let render_ok policy_set ~client =
  match Rd.render policy_set ~client with
  | Ok artifacts -> artifacts
  | Error diagnostics ->
      Alcotest.failf "render %s: %s" client
        (String.concat ", "
           (List.map (fun (diagnostic : M.diagnostic) -> diagnostic.code)
              diagnostics))

let render_error policy_set ~client =
  match Rd.render policy_set ~client with
  | Ok _ -> Alcotest.failf "render %s unexpectedly succeeded" client
  | Error diagnostics -> diagnostics

let codes diagnostics =
  List.map (fun (diagnostic : M.diagnostic) -> diagnostic.code) diagnostics

let artifact path artifacts =
  match List.find_opt (fun (item : Rd.artifact) -> item.path = path) artifacts with
  | Some item -> item
  | None -> Alcotest.failf "missing artifact %s" path

let artifact_opt path artifacts =
  List.find_opt (fun (item : Rd.artifact) -> item.path = path) artifacts

let contents_of path artifacts = (artifact path artifacts).contents

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

let has_absolute_path text =
  contains_sub text "/home/" || contains_sub text "/Users/"
  || contains_sub text "/private/" || contains_sub text "/tmp/"

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let jstring = function `String text -> text | _ -> ""

let jlist = function `List values -> values | _ -> []

let role_name (policy_set : Co.t) =
  Option.map (fun (role : M.role) -> role.name) policy_set.Co.role

let fragments (policy_set : Co.t) =
  List.map
    (fun (fragment : Co.fragment) -> (fragment.path, fragment.digest))
    policy_set.Co.fragments

let exception_ids (policy_set : Co.t) =
  List.map (fun (exception_ : M.exception_) -> exception_.id)
    policy_set.Co.exceptions

let release_with_artifacts (policy_set : Co.t) artifacts =
  R.build ~client:policy_set.Co.adapter.id ?role:(role_name policy_set)
    ?project:policy_set.Co.project ~fragments:(fragments policy_set)
    ~exceptions:(exception_ids policy_set)
    ~artifacts:
      (List.map
         (fun (item : Rd.artifact) -> (item.path, item.contents))
         artifacts)
    policy_set.Co.source

let sort_unique values = List.sort_uniq String.compare values

(* ------------------------------------------------------------------ *)
(* Real corpus: generic                                                *)
(* ------------------------------------------------------------------ *)

let test_generic_real_corpus () =
  let source = load_source () in
  let policy_set = resolve_ok ~role:"implementer" ~client:"generic-agents" source in
  let artifacts = render_ok policy_set ~client:"generic-agents" in
  let agents = contents_of "AGENTS.md" artifacts in
  let header =
    Printf.sprintf
      "<!-- nomia:generated release=0.1.0 digest=%s -->\n\
       <!-- Edit the reviewed nomia source, not this generated file. -->"
      policy_set.Co.digest
  in
  Alcotest.(check bool) "generated header starts the file" true
    (String.length agents > String.length header
    && String.sub agents 0 (String.length header) = header);
  Alcotest.(check bool) "organization heading present" true
    (contains_sub agents "# Organization agent policy (release 0.1.0)");
  Alcotest.(check bool) "role heading present" true
    (contains_sub agents "# Role: implementer");
  Alcotest.(check bool) "output contract present" true
    (contains_sub agents "## Output contract");
  Alcotest.(check bool) "skill pointer present" true
    (contains_sub agents
       "- `verify-change` — Select and run focused verification for a change, \
        then report exact commands and results. Use before claiming a change \
        is verified or requesting review. (installed at \
        .agents/skills/verify-change)");
  let ordered =
    [ "# Organization agent policy"; "# Role: implementer"; "# Skills" ]
  in
  let positions =
    List.map
      (fun needle ->
        let rec find index =
          if index + String.length needle > String.length agents then
            Alcotest.failf "missing ordered section %s" needle
          else if String.sub agents index (String.length needle) = needle then
            index
          else find (index + 1)
        in
        find 0)
      ordered
  in
  let rec ascending = function
    | left :: (right :: _ as rest) ->
        Alcotest.(check bool) "sections stay in order" true (left < right);
        ascending rest
    | _ -> ()
  in
  ascending positions;
  Alcotest.(check bool) "no absolute machine path" false
    (has_absolute_path agents);
  let explain = contents_of ".nomia/explain.json" artifacts in
  Alcotest.(check string) "explain artifact is canonical explain plus newline"
    (C.to_string (Co.explain_json policy_set) ^ "\n")
    explain;
  let script =
    artifact
      ".agents/skills/verify-change/scripts/select-checks.sh"
      artifacts
  in
  Alcotest.(check int) "skill script is executable" 0o755 script.mode;
  Alcotest.(check string) "skill script bytes are copied"
    (read_file
       (Filename.concat real_root
          "skills/verify-change/scripts/select-checks.sh"))
    script.contents;
  let skill_md = artifact ".agents/skills/verify-change/SKILL.md" artifacts in
  Alcotest.(check int) "SKILL.md is not executable" 0o644 skill_md.mode;
  Alcotest.(check bool) ".agents has every skill file" true
    (List.exists
       (fun (item : Rd.artifact) ->
         item.path = ".agents/skills/review-change/tests/check-evidence-test.sh")
       artifacts)

(* ------------------------------------------------------------------ *)
(* Real corpus: OpenCode                                               *)
(* ------------------------------------------------------------------ *)

let credential_resources =
  [
    "*.env";
    "*.env.*";
    "*.pem";
    "*.key";
    "*id_rsa*";
    "*id_ed25519*";
    "*.netrc";
    "*.aws/credentials";
  ]

(* OpenCode's whole-value matcher: [*] crosses [/]. *)
let glob_match pattern value =
  let plen = String.length pattern and vlen = String.length value in
  let memo = Hashtbl.create 64 in
  let rec go p v =
    match Hashtbl.find_opt memo (p, v) with
    | Some result -> result
    | None ->
        let result =
          if p = plen then v = vlen
          else
            match pattern.[p] with
            | '*' ->
                go (p + 1) v || (v < vlen && go p (v + 1))
            | '?' -> v < vlen && go (p + 1) (v + 1)
            | c -> v < vlen && value.[v] = c && go (p + 1) (v + 1)
        in
        Hashtbl.replace memo (p, v) result;
        result
  in
  go 0 0

let test_opencode_real_corpus () =
  let source = load_source () in
  let policy_set = resolve_ok ~role:"implementer" ~client:"opencode" source in
  let artifacts = render_ok policy_set ~client:"opencode" in
  let config_text = contents_of "opencode.json" artifacts in
  let json = Yojson.Safe.from_string config_text in
  (match json with
  | `Assoc entries ->
      Alcotest.(check string) "$schema is the first key" "$schema"
        (match entries with (key, _) :: _ -> key | [] -> "")
  | _ -> Alcotest.fail "opencode.json is not an object");
  Alcotest.(check string) "$schema value"
    "https://opencode.ai/config.json"
    (member "$schema" json |> Option.value ~default:`Null |> jstring);
  let permissions =
    member "permissions" json |> Option.value ~default:(`List []) |> jlist
  in
  let field key permission =
    member key permission |> Option.value ~default:`Null |> jstring
  in
  let asks =
    List.filter (fun permission -> field "effect" permission = "ask") permissions
  in
  let denials =
    List.filter (fun permission -> field "effect" permission = "deny") permissions
  in
  Alcotest.(check int) "all credential denials are present" 8
    (List.length denials);
  Alcotest.(check int) "conservative native consent asks are present" 4
    (List.length asks);
  let resources =
    List.map
      (fun permission ->
        let action = field "action" permission in
        let denial_effect = field "effect" permission in
        let resource = field "resource" permission in
        Alcotest.(check string) "permission action is read" "read" action;
        Alcotest.(check string) "permission effect is deny" "deny" denial_effect;
        Alcotest.(check bool) "permission object has a resource" true
          (String.length resource > 0);
        resource)
      denials
  in
  Alcotest.(check (list string)) "only supported credential denials"
    (sort_unique credential_resources)
    (sort_unique resources);
  Alcotest.(check (list (pair string string)) "conservative native ask table"
     [
       ("shell", "git push *");
       ("shell", "rm -rf *");
       ("shell", "sudo *");
       ("external_directory", "*");
     ]
     (List.map
        (fun permission ->
          (field "action" permission, field "resource" permission))
        asks));
  let denied path =
    List.exists
      (fun resource -> glob_match resource path)
      resources
  in
  List.iter
    (fun path ->
      Alcotest.(check bool) ("credential path denied: " ^ path) true
        (denied path))
    [
      ".env";
      "config/.env.local";
      "/home/user/.ssh/id_rsa";
      ".ssh/id_ed25519";
      "/home/user/.aws/credentials";
      ".aws/credentials";
      "/home/user/.netrc";
    ];
  List.iter
    (fun path ->
      Alcotest.(check bool) ("ordinary path remains readable: " ^ path) false
        (denied path))
    [ "src/example.ml"; "docs/README.md"; "notes/env.txt" ];
  Alcotest.(check bool) "no consent hook invented" false
    (contains_sub config_text "host-hook"
    || contains_sub config_text "consent");
  Alcotest.(check bool) "no authority rule id invented" false
    (contains_sub config_text "operations.authority");
  let explain =
    Yojson.Safe.from_string (contents_of ".nomia/explain.json" artifacts)
  in
  let rules =
    match member "rules" explain with Some (`List rules) -> rules | _ -> []
  in
  let find_rule id =
    match
      List.find_opt
        (fun rule ->
          member "id" rule |> Option.value ~default:`Null |> jstring = id)
        rules
    with
    | Some rule -> rule
    | None -> Alcotest.failf "explain is missing rule %s" id
  in
  let support rule =
    member "support" rule |> Option.value ~default:`Null |> jstring
  in
  let unsupported rule =
    match member "unsupported_controls" rule with
    | Some (`List controls) -> List.map jstring controls
    | _ -> []
  in
  let authority = find_rule "operations.authority" in
  Alcotest.(check string) "consent rule maps native ask as partial" "partial"
    (support authority);
  Alcotest.(check (list string)) "the missing hook control remains visible"
    [ "host-hook" ] (unsupported authority);
  let credentials = find_rule "security.credentials" in
  Alcotest.(check string) "prohibition is partial, not a full claim" "partial"
    (support credentials);
  Alcotest.(check (list string)) "unsupported filesystem control remains visible"
    [ "filesystem" ] (unsupported credentials);
  Alcotest.(check int) "opencode skill script is executable" 0o755
    (artifact
       ".opencode/skills/verify-change/scripts/select-checks.sh"
       artifacts)
      .mode;
  Alcotest.(check bool) "opencode has no generic skill directory" true
    (artifact_opt ".agents/skills/verify-change/SKILL.md" artifacts = None);
  Alcotest.(check bool) "no absolute machine path" false
    (has_absolute_path (contents_of "AGENTS.md" artifacts));
  Alcotest.(check string) "opencode AGENTS.md golden"
    (read_file "golden/opencode/AGENTS.md")
    (contents_of "AGENTS.md" artifacts);
  Alcotest.(check string) "opencode.json golden"
    (read_file "golden/opencode/opencode.json")
    config_text

let test_generic_golden () =
  let source = load_source () in
  let policy_set = resolve_ok ~client:"generic-agents" source in
  let artifacts = render_ok policy_set ~client:"generic-agents" in
  Alcotest.(check string) "generic AGENTS.md golden (no role)"
    (read_file "golden/generic/AGENTS.md")
    (contents_of "AGENTS.md" artifacts)

let test_repository_fragment () =
  let source = load_source () in
  with_temp_dir (fun repo ->
      write_file (Filename.concat repo "AGENTS.fragment.md")
        "---\n\
         rules:\n\
         \  - id: repo.local-verification\n\
         \    revision: 1\n\
         \    owner: wyo-engineering\n\
         \    strength: should\n\
         \    scope: [code-change]\n\
         \    final: false\n\
         \    kind: instruction\n\
         ---\n\
         This repository verifies changed files with its own focused gate.\n";
      let policy_set =
        resolve_ok ~repo ~role:"implementer" ~client:"generic-agents" source
      in
      let artifacts = render_ok policy_set ~client:"generic-agents" in
      let agents = contents_of "AGENTS.md" artifacts in
      Alcotest.(check bool) "repository section names the fragment" true
        (contains_sub agents
           "# Repository instructions (AGENTS.fragment.md)");
      Alcotest.(check bool) "repository prose is included" true
        (contains_sub agents
           "This repository verifies changed files with its own focused gate.");
      Alcotest.(check bool) "repository rule is included" true
        (contains_sub agents "## repo.local-verification (should)");
      Alcotest.(check bool) "fragment digest is in explain" true
        (contains_sub (contents_of ".nomia/explain.json" artifacts)
           (match policy_set.Co.fragments with
           | fragment :: _ -> fragment.Co.digest
           | [] -> "sha256:missing")))

let test_project_fragments_render_in_order () =
  let source = load_source () in
  with_temp_dir (fun repo ->
      Unix.mkdir (Filename.concat repo "tuna") 0o755;
      write_file (Filename.concat repo "AGENTS.fragment.md")
        "Wyo-wide repository instructions.\n";
      write_file
        (Filename.concat (Filename.concat repo "tuna") "AGENTS.fragment.md")
        "Tuna-specific instructions.\n";
      let policy_set =
        resolve_ok ~repo ~project:"tuna" ~client:"generic-agents" source
      in
      let agents =
        contents_of "AGENTS.md"
          (render_ok policy_set ~client:"generic-agents")
      in
      Alcotest.(check bool) "root section is present" true
        (contains_sub agents "# Repository instructions (AGENTS.fragment.md)");
      Alcotest.(check bool) "project section is present" true
        (contains_sub agents
           "# Repository instructions (tuna/AGENTS.fragment.md)");
      let index_of text needle =
        let rec loop index =
          if index + String.length needle > String.length text then
            Alcotest.failf "missing %s" needle
          else if String.sub text index (String.length needle) = needle then
            index
          else loop (index + 1)
        in
        loop 0
      in
      Alcotest.(check bool) "root precedes project" true
        (index_of agents "Wyo-wide repository"
        < index_of agents "Tuna-specific instructions");
      Alcotest.(check bool) "sibling text is absent" false
        (contains_sub agents "Pricklypear uses the habitat"))

(* ------------------------------------------------------------------ *)
(* Determinism and manifest consistency                                *)
(* ------------------------------------------------------------------ *)

let compare_artifacts label left right =
  Alcotest.(check int) (label ^ " artifact count") (List.length left)
    (List.length right);
  List.iter2
    (fun (left : Rd.artifact) (right : Rd.artifact) ->
      Alcotest.(check string) (label ^ " path") left.path right.path;
      Alcotest.(check int) (label ^ " mode") left.mode right.mode;
      Alcotest.(check string) (label ^ " contents") left.contents right.contents)
    left right

let test_determinism () =
  List.iter
    (fun client ->
      let first_source = load_source () in
      let first = resolve_ok ~role:"implementer" ~client first_source in
      let second_source = load_source () in
      let second = resolve_ok ~role:"implementer" ~client second_source in
      let first_artifacts = render_ok first ~client in
      let second_artifacts = render_ok second ~client in
      compare_artifacts client first_artifacts second_artifacts;
      Alcotest.(check string) (client ^ " release digest") first.Co.digest
        second.Co.digest;
      Alcotest.(check string) (client ^ " explain json")
        (C.to_string (Co.explain_json first))
        (C.to_string (Co.explain_json second)))
    [ "generic-agents"; "opencode" ]

let test_manifest_hashes () =
  List.iter
    (fun client ->
      let source = load_source () in
      let policy_set = resolve_ok ~role:"implementer" ~client source in
      let artifacts = render_ok policy_set ~client in
      let release = release_with_artifacts policy_set artifacts in
      Alcotest.(check string) (client ^ " manifest digest equals policy digest")
        policy_set.Co.digest (R.digest release);
      let manifest =
        Yojson.Safe.from_string (R.to_string release)
      in
      let listed =
        member "artifacts" manifest |> Option.value ~default:(`List []) |> jlist
      in
      Alcotest.(check int) (client ^ " artifact count in manifest")
        (List.length artifacts) (List.length listed);
      List.iter
        (fun entry ->
          let path =
            member "path" entry |> Option.value ~default:`Null |> jstring
          in
          let expected =
            member "sha256" entry |> Option.value ~default:`Null |> jstring
          in
          Alcotest.(check bool) (client ^ " manifest excludes itself") false
            (path = ".nomia/manifest.json");
          let item = artifact path artifacts in
          Alcotest.(check string) (client ^ " artifact hash " ^ path)
            expected (C.sha256_hex item.contents))
        listed;
      let agents = contents_of "AGENTS.md" artifacts in
      Alcotest.(check bool) (client ^ " header carries release digest") true
        (contains_sub agents ("digest=" ^ policy_set.Co.digest)))
    [ "generic-agents"; "opencode" ]

(* ------------------------------------------------------------------ *)
(* Filesystem and failure behavior                                     *)
(* ------------------------------------------------------------------ *)

let test_write_modes () =
  let source = load_source () in
  let policy_set = resolve_ok ~role:"implementer" ~client:"opencode" source in
  let artifacts = render_ok policy_set ~client:"opencode" in
  with_temp_dir (fun temporary ->
      let out = Filename.concat temporary "release" in
      (match Rd.write ~out artifacts with
      | Ok () -> ()
      | Error message -> Alcotest.failf "write: %s" message);
      let script =
        Filename.concat out
          ".opencode/skills/verify-change/scripts/select-checks.sh"
      in
      let skill =
        Filename.concat out ".opencode/skills/verify-change/SKILL.md"
      in
      Alcotest.(check int) "written script mode" 0o755
        ((Unix.stat script).st_perm land 0o777);
      Alcotest.(check int) "written data mode" 0o644
        ((Unix.stat skill).st_perm land 0o777);
      Alcotest.(check string) "written bytes"
        (contents_of ".opencode/skills/verify-change/scripts/select-checks.sh"
           artifacts)
        (read_file script);
      Alcotest.(check bool) "out is no longer empty" true
        (Array.length (Sys.readdir out) > 0);
      (match Rd.check_out ~out with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "non-empty out passed check_out");
      (match Rd.write ~out artifacts with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "non-empty out was overwritten"))

let test_write_refuses_file_and_escape () =
  with_temp_dir (fun temporary ->
      let file = Filename.concat temporary "not-a-dir" in
      write_file file "sentinel";
      (match Rd.check_out ~out:file with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "regular file passed check_out");
      let out = Filename.concat temporary "out" in
      let escape : Rd.artifact =
        { path = "../escape.txt"; contents = "no"; mode = 0o644 }
      in
      (match Rd.write ~out [ escape ] with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "escaping path was written");
      Alcotest.(check bool) "no file escaped out" false
        (Sys.file_exists (Filename.concat temporary "escape.txt")))

let test_write_empty_out_is_untouched_on_refusal () =
  with_temp_dir (fun temporary ->
      let out = Filename.concat temporary "out" in
      Unix.mkdir out 0o755;
      let sentinel = Filename.concat out "sentinel" in
      write_file sentinel "keep";
      let source = load_source () in
      let policy_set = resolve_ok ~client:"generic-agents" source in
      let artifacts = render_ok policy_set ~client:"generic-agents" in
      (match Rd.write ~out artifacts with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "non-empty out accepted");
      Alcotest.(check string) "sentinel unchanged" "keep" (read_file sentinel);
      Alcotest.(check bool) "no artifact written" false
        (Sys.file_exists (Filename.concat out "AGENTS.md")))

(* ------------------------------------------------------------------ *)
(* Diagnostics                                                         *)
(* ------------------------------------------------------------------ *)

let test_unknown_client () =
  let source = load_source () in
  let policy_set = resolve_ok ~client:"generic-agents" source in
  let diagnostics = render_error policy_set ~client:"not-a-client" in
  Alcotest.(check bool) "unknown client code" true
    (List.mem "render.client.unknown" (codes diagnostics));
  let mismatched = render_error policy_set ~client:"opencode" in
  Alcotest.(check bool) "client/adapter mismatch code" true
    (List.mem "render.client.mismatch" (codes mismatched))

let test_skill_hash_mismatch () =
  let source = load_source () in
  let changed =
    {
      source with
      M.skills =
        List.map
          (fun (skill : M.skill) ->
            if skill.name <> "verify-change" then skill
            else
              {
                skill with
                files =
                  List.map
                    (fun (path, hash) ->
                      if path = "SKILL.md" then (path, String.make 64 '0')
                      else (path, hash))
                    skill.files;
              })
          source.skills;
    }
  in
  let policy_set = resolve_ok ~role:"implementer" ~client:"generic-agents" changed in
  let diagnostics = render_error policy_set ~client:"generic-agents" in
  Alcotest.(check bool) "skill hash mismatch code" true
    (List.mem "render.skill.hash" (codes diagnostics));
  Alcotest.(check bool) "diagnostic names the skill file" true
    (List.exists
       (fun (diagnostic : M.diagnostic) ->
         diagnostic.code = "render.skill.hash"
         && contains_sub diagnostic.message "skills/verify-change/SKILL.md")
       diagnostics)

(* ------------------------------------------------------------------ *)
(* Suite                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia render"
    [
      ( "generic",
        [
          Alcotest.test_case "real corpus" `Quick test_generic_real_corpus;
          Alcotest.test_case "golden without role" `Quick test_generic_golden;
          Alcotest.test_case "repository fragment" `Quick
            test_repository_fragment;
          Alcotest.test_case "project fragments render in order" `Quick
            test_project_fragments_render_in_order;
        ] );
      ( "opencode",
        [ Alcotest.test_case "real corpus" `Quick test_opencode_real_corpus ] );
      ( "release",
        [
          Alcotest.test_case "deterministic artifacts" `Quick test_determinism;
          Alcotest.test_case "manifest hashes" `Quick test_manifest_hashes;
        ] );
      ( "filesystem",
        [
          Alcotest.test_case "modes and non-empty out" `Quick test_write_modes;
          Alcotest.test_case "file and escaping path" `Quick
            test_write_refuses_file_and_escape;
          Alcotest.test_case "refusal leaves out untouched" `Quick
            test_write_empty_out_is_untouched_on_refusal;
        ] );
      ( "diagnostics",
        [
          Alcotest.test_case "unknown client" `Quick test_unknown_client;
          Alcotest.test_case "skill hash mismatch" `Quick
            test_skill_hash_mismatch;
        ] );
    ]
