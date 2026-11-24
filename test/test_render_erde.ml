open Nomia

module C = Nomia.Canonical
module Co = Nomia.Compose
module M = Nomia.Model
module R = Nomia.Release
module Rd = Nomia.Render
module S = Nomia.Source

let real_root = ".."
let today = "2030-01-01"
let memory_fragment = "fragments/erde-memory-contract.md"

(* [erde] is its own dune project, excluded from the root project's build
   context, so the test locates the checked-in source file upward from the
   test's working directory (or via dune's source root). This is deliberately
   a direct source read: it is the cross-project drift check the card asks
   for, not a copied fixture that could silently agree with itself. *)
let rec find_up dir relative =
  let candidate = Filename.concat dir relative in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_up parent relative

let erde_prompt =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root when Sys.file_exists (Filename.concat root "erde/lib/prompt.ml") ->
      Filename.concat root "erde/lib/prompt.ml"
  | _ -> (
      match find_up (Sys.getcwd ()) "erde/lib/prompt.ml" with
      | Some path -> path
      | None ->
          Alcotest.fail
            "cannot locate erde/lib/prompt.ml from the test working directory")

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
  let temporary = Filename.temp_dir "nomia-erde-" "" in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree temporary with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () -> action temporary)

let rec copy_path source destination =
  if Sys.is_directory source then begin
    if not (Sys.file_exists destination) then Unix.mkdir destination 0o755;
    Sys.readdir source
    |> Array.iter (fun name ->
           copy_path (Filename.concat source name)
             (Filename.concat destination name))
  end
  else write_file destination (read_file source)

let copy_source_tree destination =
  List.iter
    (fun entry ->
      copy_path (Filename.concat real_root entry)
        (Filename.concat destination entry))
    [
      "policy.yaml";
      "exceptions.yaml";
      "rules";
      "roles";
      "adapters";
      "skills";
      "fragments";
    ]

let load_source () =
  match S.load_and_validate ~root:real_root ~today with
  | Ok source -> source
  | Error diagnostics ->
      Alcotest.failf "load %s: %s" real_root
        (String.concat ", "
           (List.map (fun (diagnostic : M.diagnostic) -> diagnostic.code)
              diagnostics))

let resolve_ok ?repo ?role ?client source =
  match Co.resolve ?repo ?role ?client ~today source with
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

let index_of hay needle =
  let hay_len = String.length hay and needle_len = String.length needle in
  let rec loop index =
    if index + needle_len > hay_len then
      Alcotest.failf "missing ordered text %s" needle
    else if String.sub hay index needle_len = needle then index
    else loop (index + 1)
  in
  loop 0

let assert_ascending text needles =
  let rec loop index = function
    | left :: (right :: _ as rest) ->
        Alcotest.(check bool)
          (Printf.sprintf "section %d stays in order" index)
          true
          (index_of text left < index_of text right);
        loop (index + 1) rest
    | _ -> ()
  in
  loop 0 needles

let after_marker text marker =
  let start = index_of text marker in
  String.sub text start (String.length text - start)

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let jstring = function `String text -> text | _ -> ""
let jlist = function `List values -> values | _ -> []

let json_string key json =
  member key json |> Option.value ~default:`Null |> jstring

let role_name (policy_set : Co.t) =
  Option.map (fun (role : M.role) -> role.name) policy_set.Co.role

(* ------------------------------------------------------------------ *)
(* The reviewed contract and the Erde raw string                       *)
(* ------------------------------------------------------------------ *)

let between text ~open_marker ~close_marker =
  match
    let open_len = String.length open_marker in
    let rec find index =
      if index + open_len > String.length text then None
      else if String.sub text index open_len = open_marker then Some index
      else find (index + 1)
    in
    find 0
  with
  | None -> Alcotest.failf "missing %s" open_marker
  | Some start ->
      let content_start = start + String.length open_marker in
      let close_len = String.length close_marker in
      let rec find index =
        if index + close_len > String.length text then None
        else if String.sub text index close_len = close_marker then Some index
        else find (index + 1)
      in
      (match find content_start with
      | None -> Alcotest.failf "missing close marker for %s" open_marker
      | Some stop -> String.sub text content_start (stop - content_start))

let memory_contract_text () =
  between (read_file erde_prompt) ~open_marker:"let memory_policy_v1 =\n{|"
    ~close_marker:"|}"

(* ------------------------------------------------------------------ *)
(* Real corpus: erde without a role                                    *)
(* ------------------------------------------------------------------ *)

let header_of (policy_set : Co.t) =
  Printf.sprintf
    "<!-- nomia:generated release=%s digest=%s -->\n\
     <!-- Edit the reviewed nomia source, not this generated file. -->"
    policy_set.Co.source.policy.release policy_set.Co.digest

let test_erde_no_role () =
  let memory_text = memory_contract_text () in
  let source = load_source () in
  let policy_set = resolve_ok ~client:"erde" source in
  let artifacts = render_ok policy_set ~client:"erde" in
  let org_core = contents_of "org-core.md" artifacts in
  let framework = contents_of "framework.md" artifacts in
  let policy_text = contents_of "policy.json" artifacts in
  let header = header_of policy_set in
  Alcotest.(check bool) "org-core starts with the generated header" true
    (String.length org_core > String.length header
    && String.sub org_core 0 (String.length header) = header);
  Alcotest.(check bool) "framework starts with the generated header" true
    (String.length framework > String.length header
    && String.sub framework 0 (String.length header) = header);
  Alcotest.(check bool) "org-core carries the organization heading" true
    (contains_sub org_core "# Organization agent policy (release 0.2.0)");
  Alcotest.(check bool) "org-core carries rules by id" true
    (contains_sub org_core "## engineering.verify-change (must, final)");
  Alcotest.(check bool) "org-core has no memory contract" false
    (contains_sub org_core "MEMORY LIFECYCLE");
  Alcotest.(check bool) "org-core has no role section" false
    (contains_sub org_core "## Role:");
  Alcotest.(check bool) "framework has the memory heading" true
    (contains_sub framework
       "## Shared harness and memory lifecycle contract");
  Alcotest.(check bool) "framework carries the exact memory contract" true
    (contains_sub framework memory_text);
  Alcotest.(check bool) "framework has procedure pointers" true
    (contains_sub framework "# Procedures");
  Alcotest.(check bool) "framework has no role without a selection" false
    (contains_sub framework "## Role:");
  assert_ascending framework
    [
      header;
      "# Organization agent policy (release 0.2.0)";
      "## Shared harness and memory lifecycle contract";
      memory_text;
      "# Procedures";
    ];
  Alcotest.(check bool) "erde does not emit AGENTS.md" true
    (artifact_opt "AGENTS.md" artifacts = None);
  let explain = contents_of ".nomia/explain.json" artifacts in
  Alcotest.(check string) "explain artifact is canonical explain plus newline"
    (C.to_string (Co.explain_json policy_set) ^ "\n")
    explain;
  let policy = Yojson.Safe.from_string policy_text in
  Alcotest.(check string) "policy.json is canonical" policy_text
    (C.to_string policy ^ "\n");
  Alcotest.(check string) "policy schema" "nomia-erde/v1"
    (json_string "schema" policy);
  Alcotest.(check string) "policy release" "0.2.0" (json_string "release" policy);
  Alcotest.(check string) "policy compiler" "agentctl/1"
    (json_string "compiler" policy);
  Alcotest.(check string) "policy digest" policy_set.Co.digest
    (json_string "policy_digest" policy);
  Alcotest.(check string) "source digest" policy_set.Co.source_digest
    (json_string "source_digest" policy);
  Alcotest.(check string) "org-core hash" (C.sha256_hex org_core)
    (json_string "org_core_sha256" policy);
  Alcotest.(check string) "memory-contract hash" (C.sha256_hex memory_text)
    (json_string "memory_contract_sha256" policy);
  Alcotest.(check string) "framework hash" (C.sha256_hex framework)
    (json_string "framework_sha256" policy);
  Alcotest.(check bool) "no role is a JSON null" true
    (member "role" policy = Some `Null);
  Alcotest.(check bool) "policy json has no timestamp field" false
    (contains_sub policy_text "timestamp"
    || contains_sub policy_text "generated_at"
    || contains_sub policy_text "2030-01-01");
  Alcotest.(check bool) "policy json has no absolute path" false
    (has_absolute_path policy_text);
  List.iter
    (fun (item : Rd.artifact) ->
      Alcotest.(check bool) (item.path ^ " has no absolute path") false
        (has_absolute_path item.contents);
      Alcotest.(check bool) (item.path ^ " has no wall-clock date") false
        (contains_sub item.contents today))
    artifacts;
  let script =
    artifact "skills/verify-change/scripts/select-checks.sh" artifacts
  in
  Alcotest.(check int) "erde skill script is executable" 0o755 script.mode;
  Alcotest.(check string) "erde skill script bytes are copied"
    (read_file
       (Filename.concat real_root
          "skills/verify-change/scripts/select-checks.sh"))
    script.contents;
  Alcotest.(check int) "erde SKILL.md is not executable" 0o644
    (artifact "skills/verify-change/SKILL.md" artifacts).mode

(* ------------------------------------------------------------------ *)
(* Real corpus: erde with roles                                        *)
(* ------------------------------------------------------------------ *)

let test_erde_roles () =
  let memory_text = memory_contract_text () in
  let source = load_source () in
  let baseline_org_core =
    contents_of "org-core.md" (render_ok (resolve_ok ~client:"erde" source) ~client:"erde")
  in
  let frameworks = ref [] in
  List.iter
    (fun role_name ->
      let policy_set = resolve_ok ~role:role_name ~client:"erde" source in
      let artifacts = render_ok policy_set ~client:"erde" in
      let framework = contents_of "framework.md" artifacts in
      let org_core = contents_of "org-core.md" artifacts in
      let policy = Yojson.Safe.from_string (contents_of "policy.json" artifacts) in
      let role =
        match policy_set.Co.role with
        | Some role -> role
        | None -> Alcotest.fail "role was not selected"
      in
      frameworks := (role_name, framework) :: !frameworks;
      Alcotest.(check bool) (role_name ^ " role heading") true
        (contains_sub framework ("## Role: " ^ role_name));
      Alcotest.(check bool) (role_name ^ " charter text") true
        (contains_sub framework (String.trim role.charter));
      Alcotest.(check bool) (role_name ^ " output contract heading") true
        (contains_sub framework "### Output contract");
      Alcotest.(check bool) (role_name ^ " output contract text") true
        (contains_sub framework (String.trim role.output_contract));
      Alcotest.(check bool) (role_name ^ " keeps the memory contract") true
        (contains_sub framework memory_text);
      assert_ascending framework
        [
          "## Shared harness and memory lifecycle contract";
          memory_text;
          "## Role: " ^ role_name;
          "### Output contract";
          "# Procedures";
        ];
      Alcotest.(check bool) (role_name ^ " org-core rules ignore the role") true
        (after_marker org_core "# Organization agent policy"
        = after_marker baseline_org_core "# Organization agent policy");
      let role_json =
        member "role" policy |> Option.value ~default:`Null
      in
      Alcotest.(check string) (role_name ^ " policy role name") role_name
        (json_string "name" role_json);
      let source_hash =
        List.assoc_opt role.source source.files
        |> Option.value ~default:""
      in
      Alcotest.(check bool) (role_name ^ " role source is hashed") true
        (String.length source_hash = 64);
      Alcotest.(check string) (role_name ^ " role source hash") source_hash
        (json_string "role_sha256" role_json);
      Alcotest.(check string) (role_name ^ " charter hash")
        (C.sha256_hex role.charter)
        (json_string "charter_sha256" role_json);
      Alcotest.(check string) (role_name ^ " output contract hash")
        (C.sha256_hex role.output_contract)
        (json_string "output_contract_sha256" role_json);
      Alcotest.(check string) (role_name ^ " framework hash")
        (C.sha256_hex framework)
        (json_string "framework_sha256" policy))
    [ "implementer"; "code-reviewer" ];
  match !frameworks with
  | [ (_, implementer); (_, reviewer) ] ->
      Alcotest.(check bool) "roles produce different frameworks" true
        (implementer <> reviewer)
  | _ -> Alcotest.fail "expected two rendered frameworks"

(* ------------------------------------------------------------------ *)
(* Real corpus: erde with a repository fragment                        *)
(* ------------------------------------------------------------------ *)

let test_erde_repository_fragment () =
  let source = load_source () in
  with_temp_dir (fun repo ->
      write_file (Filename.concat repo "AGENTS.fragment.md")
        "---\n\
         rules:\n\
         \  - id: repo.erde-verification\n\
         \    revision: 1\n\
         \    owner: wyo-engineering\n\
         \    strength: should\n\
         \    scope: [code-change]\n\
         \    final: false\n\
         \    kind: instruction\n\
         ---\n\
         This repository verifies changed files with its own focused gate.\n";
      let policy_set =
        resolve_ok ~repo ~role:"implementer" ~client:"erde" source
      in
      let artifacts = render_ok policy_set ~client:"erde" in
      let framework = contents_of "framework.md" artifacts in
      let org_core = contents_of "org-core.md" artifacts in
      Alcotest.(check bool) "repository prose is included" true
        (contains_sub framework
           "This repository verifies changed files with its own focused gate.");
      Alcotest.(check bool) "repository rule is included" true
        (contains_sub framework "## repo.erde-verification (should)");
      Alcotest.(check bool) "repository section names the fragment" true
        (contains_sub framework
           "## Repository instructions (AGENTS.fragment.md)");
      assert_ascending framework
        [
          "## Role: implementer";
          "### Output contract";
          "## Repository instructions (AGENTS.fragment.md)";
          "## repo.erde-verification (should)";
          "# Procedures";
        ];
      Alcotest.(check bool) "org-core never carries repository prose" false
        (contains_sub org_core
           "This repository verifies changed files with its own focused gate."))

(* ------------------------------------------------------------------ *)
(* Determinism                                                         *)
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
  let first_source = load_source () in
  let second_source = load_source () in
  List.iter
    (fun role ->
      let first = resolve_ok ?role ~client:"erde" first_source in
      let second = resolve_ok ?role ~client:"erde" second_source in
      compare_artifacts
        (match role with None -> "no role" | Some role -> role)
        (render_ok first ~client:"erde")
        (render_ok second ~client:"erde");
      Alcotest.(check string) "release digest is stable" first.Co.digest
        second.Co.digest)
    [ None; Some "implementer"; Some "code-reviewer" ]

(* ------------------------------------------------------------------ *)
(* Source participation                                                *)
(* ------------------------------------------------------------------ *)

let test_fragment_is_source_and_moves_the_digest () =
  let memory_text = memory_contract_text () in
  let fragment_path = Filename.concat real_root memory_fragment in
  Alcotest.(check string) "checked-in fragment equals memory_policy_v1"
    memory_text (read_file fragment_path);
  let source = load_source () in
  let recorded =
    List.assoc_opt memory_fragment source.files
    |> Option.value ~default:""
  in
  Alcotest.(check string) "fragment hash is the memory contract hash"
    (S.sha256_hex memory_text) recorded;
  Alcotest.(check string) "fragment participates in the release source digest"
    (C.sha256_hex memory_text)
    (List.assoc memory_fragment source.files);
  with_temp_dir (fun temporary ->
      copy_source_tree temporary;
      let before = S.load ~root:temporary in
      let before =
        match before with
        | Ok source -> source
        | Error _ -> Alcotest.fail "temporary source did not load"
      in
      let before_digest = R.source_digest before in
      let before_hash = List.assoc memory_fragment before.files in
      let edited_path = Filename.concat temporary memory_fragment in
      write_file edited_path (read_file edited_path ^ " ");
      let after = S.load ~root:temporary in
      let after =
        match after with
        | Ok source -> source
        | Error _ -> Alcotest.fail "edited temporary source did not load"
      in
      Alcotest.(check bool) "editing the fragment changes its hash" true
        (List.assoc memory_fragment after.files <> before_hash);
      Alcotest.(check bool) "editing the fragment changes the source digest"
        true
        (R.source_digest after <> before_digest);
      let policy_set = resolve_ok ~client:"erde" after in
      let artifacts = render_ok policy_set ~client:"erde" in
      let policy = Yojson.Safe.from_string (contents_of "policy.json" artifacts) in
      Alcotest.(check string) "policy names the edited memory contract"
        (S.sha256_hex (read_file edited_path))
        (json_string "memory_contract_sha256" policy);
      Alcotest.(check bool) "the edited fragment is what framework carries" true
        (contains_sub (contents_of "framework.md" artifacts)
           (read_file edited_path)))

(* ------------------------------------------------------------------ *)
(* CLI                                                                 *)
(* ------------------------------------------------------------------ *)

let start_dir = Sys.getcwd ()
let binary = Filename.concat start_dir "../bin/agentctl.exe"

type process_result = { code : int; stdout : string; stderr : string }

let run_agentctl args =
  let out_path = Filename.temp_file "agentctl-erde-stdout-" ".log" in
  let err_path = Filename.temp_file "agentctl-erde-stderr-" ".log" in
  let out_fd = Unix.openfile out_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let err_fd = Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let argv = Array.of_list (binary :: args) in
  let pid = Unix.create_process binary argv Unix.stdin out_fd err_fd in
  Unix.close out_fd;
  Unix.close err_fd;
  let _, status = Unix.waitpid [] pid in
  let code =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED signal -> 128 + signal
  in
  let stdout = read_file out_path and stderr = read_file err_path in
  Sys.remove out_path;
  Sys.remove err_path;
  { code; stdout; stderr }

let rec walk_files root relative =
  let directory = if relative = "" then root else Filename.concat root relative in
  Sys.readdir directory |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
         let child = if relative = "" then name else relative ^ "/" ^ name in
         let full = Filename.concat root child in
         if Sys.is_directory full then walk_files root child else [ child ])

let snapshot root =
  walk_files root ""
  |> List.map (fun relative ->
         let full = Filename.concat root relative in
         (relative, (Unix.stat full).st_perm land 0o777, read_file full))

let test_cli_erde () =
  Unix.putenv "NOMIA_TODAY" today;
  with_temp_dir (fun temporary ->
      let first = Filename.concat temporary "first" in
      let second = Filename.concat temporary "second" in
      Unix.mkdir first 0o755;
      Unix.mkdir second 0o755;
      let render_args out =
        [
          "render";
          "--client";
          "erde";
          "--role";
          "code-reviewer";
          "--source";
          real_root;
          "--out";
          out;
        ]
      in
      let first_result = run_agentctl (render_args first) in
      Alcotest.(check int) "first erde render exits 0" 0 first_result.code;
      Alcotest.(check string) "first erde render is quiet" "" first_result.stderr;
      let second_result = run_agentctl (render_args second) in
      Alcotest.(check int) "second erde render exits 0" 0 second_result.code;
      Alcotest.(check string) "second erde render is quiet" "" second_result.stderr;
      Alcotest.(check (list (triple string int string)))
        "two erde renders are byte identical"
        (snapshot first) (snapshot second);
      let explained =
        run_agentctl
          [
            "explain";
            "--client";
            "erde";
            "--role";
            "code-reviewer";
            "--source";
            real_root;
            "--json";
          ]
      in
      Alcotest.(check int) "erde explain exits 0" 0 explained.code;
      Alcotest.(check string) "explain json equals rendered explain bytes"
        (read_file (Filename.concat first ".nomia/explain.json"))
        explained.stdout;
      let explain = Yojson.Safe.from_string explained.stdout in
      let explain_digest =
        member "release" explain |> Option.value ~default:`Null
        |> fun release -> json_string "digest" release
      in
      let policy =
        Yojson.Safe.from_string (read_file (Filename.concat first "policy.json"))
      in
      Alcotest.(check string) "policy.json and explain agree on the digest"
        explain_digest (json_string "policy_digest" policy);
      Alcotest.(check bool) "manifest was written" true
        (Sys.file_exists (Filename.concat first ".nomia/manifest.json")))

(* ------------------------------------------------------------------ *)
(* Suite                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia erde render"
    [
      ( "render",
        [
          Alcotest.test_case "no role" `Quick test_erde_no_role;
          Alcotest.test_case "implementer and reviewer" `Quick test_erde_roles;
          Alcotest.test_case "repository fragment" `Quick
            test_erde_repository_fragment;
        ] );
      ( "release",
        [
          Alcotest.test_case "deterministic artifacts" `Quick test_determinism;
          Alcotest.test_case "fragment participates in the digest" `Quick
            test_fragment_is_source_and_moves_the_digest;
        ] );
      ("cli", [ Alcotest.test_case "render twice and explain" `Quick test_cli_erde ]);
    ]
