open Nomia

module C = Nomia.Canonical
module Co = Nomia.Compose
module M = Nomia.Model
module R = Nomia.Release
module Rd = Nomia.Render
module S = Nomia.Source

let real_root = ".."
let today = "2030-01-01"
let template_rel = "fragments/saguaro-persona-template.md"

let rec find_up dir relative =
  let candidate = Filename.concat dir relative in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_up parent relative

(* [saguaro] owns the persona template outside nomia, so this is a
   deliberate direct read of the product source, not a copy that could agree
   with itself. *)
let saguaro_template =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root
    when Sys.file_exists
           (Filename.concat root "saguaro/harness/persona.tmpl.md") ->
      Filename.concat root "saguaro/harness/persona.tmpl.md"
  | _ -> (
      match find_up (Sys.getcwd ()) "saguaro/harness/persona.tmpl.md" with
      | Some path -> path
      | None ->
          Alcotest.fail
            "cannot locate saguaro/harness/persona.tmpl.md from the test working \
             directory")

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
  let temporary = Filename.temp_dir "nomia-saguaro-" "" in
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
    [ "policy.yaml"; "exceptions.yaml"; "rules"; "roles"; "adapters"; "skills";
      "fragments" ]

let load_source ?(root = real_root) () =
  match S.load_and_validate ~root ~today with
  | Ok source -> source
  | Error diagnostics ->
      Alcotest.failf "load %s: %s" root
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

let after_marker text marker =
  let start = index_of text marker in
  String.sub text start (String.length text - start)

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let jstring = function `String text -> text | _ -> ""

let json_string key json =
  member key json |> Option.value ~default:`Null |> jstring

let role_name (policy_set : Co.t) =
  Option.map (fun (role : M.role) -> role.name) policy_set.Co.role

let release_with_artifacts (policy_set : Co.t) artifacts =
  let fragments =
    List.map
      (fun (fragment : Co.fragment) -> (fragment.path, fragment.digest))
      policy_set.Co.fragments
  in
  R.build ~client:policy_set.Co.adapter.id ?role:(role_name policy_set)
    ?project:policy_set.Co.project ~fragments ~exceptions:
      (List.map (fun (exception_ : M.exception_) -> exception_.id)
         policy_set.Co.exceptions)
    ~artifacts:
      (List.map
         (fun (item : Rd.artifact) -> (item.path, item.contents))
         artifacts)
    policy_set.Co.source

let organization_rules (policy_set : Co.t) =
  List.filter
    (fun (resolved : Co.resolved_rule) -> resolved.origin = Co.Organization)
    policy_set.Co.rules

(* ------------------------------------------------------------------ *)
(* Real corpus                                                        *)
(* ------------------------------------------------------------------ *)

let test_saguaro_real_corpus () =
  let source = load_source () in
  let policy_set = resolve_ok ~client:"saguaro" source in
  let artifacts = render_ok policy_set ~client:"saguaro" in
  let persona = contents_of "persona.md" artifacts in
  let policy_text = contents_of "policy.json" artifacts in
  let fragment_text =
    read_file (Filename.concat real_root template_rel)
  in
  let product_template = read_file saguaro_template in
  Alcotest.(check string)
    "reviewed fragment is byte-identical to saguaro/harness/persona.tmpl.md"
    product_template fragment_text;
  Alcotest.(check string) "reviewed fragment hash"
    (C.sha256_hex product_template)
    (List.assoc template_rel source.M.files);
  let header =
    Printf.sprintf
      "<!-- nomia:generated release=0.1.0 digest=%s -->\n\
       <!-- Edit the reviewed nomia source, not this generated persona. -->"
      policy_set.Co.digest
  in
  Alcotest.(check bool) "generated block header present" true
    (contains_sub persona header);
  Alcotest.(check bool) "organization baseline heading present" true
    (contains_sub persona "## Organization baseline (release 0.1.0)");
  List.iter
    (fun (resolved : Co.resolved_rule) ->
      let rule = resolved.rule in
      let heading =
        if rule.final then
          Printf.sprintf "## %s (%s, final)" rule.id
            (match rule.strength with
            | M.Must -> "must"
            | M.Should -> "should"
            | M.May -> "may")
        else
          Printf.sprintf "## %s (%s)" rule.id
            (match rule.strength with
            | M.Must -> "must"
            | M.Should -> "should"
            | M.May -> "may")
      in
      Alcotest.(check bool) ("persona carries " ^ heading) true
        (contains_sub persona heading))
    (organization_rules policy_set);
  Alcotest.(check bool) "a known organization body is present" true
    (contains_sub persona
       "Read the applicable repository instructions and the authoritative \
        specification");
  List.iter
    (fun placeholder ->
      Alcotest.(check bool) (placeholder ^ " placeholder is preserved") true
        (contains_sub persona placeholder))
    [ "{{OWNER}}"; "{{AGENT}}"; "{{HOST}}" ];
  Alcotest.(check bool) "voice heading is still present" true
    (contains_sub persona "## Voice");
  Alcotest.(check bool) "policy block precedes the voice heading" true
    (index_of persona "## Organization baseline"
    < index_of persona "## Voice");
  Alcotest.(check bool) "voice text is unchanged after insertion" true
    (after_marker persona "## Voice" = after_marker fragment_text "## Voice");
  Alcotest.(check bool) "original voice bullet survives" true
    (contains_sub persona
       "- Lead with the answer. Then the interesting part, if there is one.");
  Alcotest.(check bool) "template's generated-by marker survives" true
    (contains_sub persona "<!-- Generated by scripts/gen_agents.py");
  Alcotest.(check bool) "saguaro does not emit AGENTS.md" true
    (artifact_opt "AGENTS.md" artifacts = None);
  let explain = contents_of ".nomia/explain.json" artifacts in
  Alcotest.(check string) "explain artifact is canonical explain plus newline"
    (C.to_string (Co.explain_json policy_set) ^ "\n")
    explain;
  let policy = Yojson.Safe.from_string policy_text in
  Alcotest.(check string) "policy.json is canonical" policy_text
    (C.to_string policy ^ "\n");
  Alcotest.(check string) "policy schema" "nomia-saguaro/v1"
    (json_string "schema" policy);
  Alcotest.(check string) "policy release" "0.1.0" (json_string "release" policy);
  Alcotest.(check string) "policy compiler" "agentctl/1"
    (json_string "compiler" policy);
  Alcotest.(check string) "policy digest" policy_set.Co.digest
    (json_string "policy_digest" policy);
  Alcotest.(check string) "source digest" policy_set.Co.source_digest
    (json_string "source_digest" policy);
  Alcotest.(check string) "template hash" (C.sha256_hex fragment_text)
    (json_string "template_sha256" policy);
  Alcotest.(check string) "persona hash" (C.sha256_hex persona)
    (json_string "persona_sha256" policy);
  Alcotest.(check bool) "personalized is false" true
    (member "personalized" policy = Some (`Bool false));
  Alcotest.(check bool) "no role is a JSON null" true
    (member "role" policy = Some `Null);
  Alcotest.(check bool) "policy json has no timestamp" false
    (contains_sub policy_text "timestamp"
    || contains_sub policy_text "generated_at"
    || contains_sub policy_text today);
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
  Alcotest.(check int) "saguaro skill script is executable" 0o755 script.mode;
  Alcotest.(check string) "saguaro skill script bytes are copied"
    (read_file
       (Filename.concat real_root
          "skills/verify-change/scripts/select-checks.sh"))
    script.contents;
  Alcotest.(check int) "saguaro SKILL.md is not executable" 0o644
    (artifact "skills/verify-change/SKILL.md" artifacts).mode;
  let release = release_with_artifacts policy_set artifacts in
  Alcotest.(check string) "manifest digest equals policy digest"
    policy_set.Co.digest (R.digest release)

let test_saguaro_golden () =
  let source = load_source () in
  let policy_set = resolve_ok ~client:"saguaro" source in
  let artifacts = render_ok policy_set ~client:"saguaro" in
  Alcotest.(check string) "saguaro persona golden (no role)"
    (read_file "golden/saguaro/persona.md")
    (contents_of "persona.md" artifacts)

(* ------------------------------------------------------------------ *)
(* Determinism                                                        *)
(* ------------------------------------------------------------------ *)

let test_determinism () =
  let first_source = load_source () in
  let second_source = load_source () in
  let first = resolve_ok ~client:"saguaro" first_source in
  let second = resolve_ok ~client:"saguaro" second_source in
  let first_artifacts = render_ok first ~client:"saguaro" in
  let second_artifacts = render_ok second ~client:"saguaro" in
  Alcotest.(check int) "artifact count" (List.length first_artifacts)
    (List.length second_artifacts);
  List.iter2
    (fun (left : Rd.artifact) (right : Rd.artifact) ->
      Alcotest.(check string) "path" left.path right.path;
      Alcotest.(check int) "mode" left.mode right.mode;
      Alcotest.(check string) "contents" left.contents right.contents)
    first_artifacts second_artifacts;
  Alcotest.(check string) "release digest is stable" first.Co.digest
    second.Co.digest

(* ------------------------------------------------------------------ *)
(* Template integrity                                                 *)
(* ------------------------------------------------------------------ *)

let test_template_drift_is_loud () =
  with_temp_dir (fun temporary ->
      copy_source_tree temporary;
      let loaded = load_source ~root:temporary () in
      let before_render =
        render_ok (resolve_ok ~client:"saguaro" loaded) ~client:"saguaro"
      in
      Alcotest.(check bool) "temporary source renders" true
        (List.length before_render > 0);
      let path = Filename.concat temporary template_rel in
      write_file path (read_file path ^ " ");
      let policy_set = resolve_ok ~client:"saguaro" loaded in
      match Rd.render policy_set ~client:"saguaro" with
      | Ok _ -> Alcotest.fail "drifted template rendered without a diagnostic"
      | Error diagnostics ->
          Alcotest.(check bool) "drift is render.fragment.hash" true
            (List.exists
               (fun (diagnostic : M.diagnostic) ->
                 diagnostic.code = "render.fragment.hash"
                 && diagnostic.path = template_rel)
               diagnostics))

(* ------------------------------------------------------------------ *)
(* CLI                                                                 *)
(* ------------------------------------------------------------------ *)

let start_dir = Sys.getcwd ()
let binary = Filename.concat start_dir "../bin/agentctl.exe"

type process_result = { code : int; stdout : string; stderr : string }

let run_agentctl args =
  let out_path = Filename.temp_file "agentctl-saguaro-stdout-" ".log" in
  let err_path = Filename.temp_file "agentctl-saguaro-stderr-" ".log" in
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

let test_cli_saguaro () =
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
          "saguaro";
          "--source";
          real_root;
          "--out";
          out;
        ]
      in
      let first_result = run_agentctl (render_args first) in
      Alcotest.(check int) "first saguaro render exits 0" 0 first_result.code;
      Alcotest.(check string) "first saguaro render is quiet" ""
        first_result.stderr;
      let second_result = run_agentctl (render_args second) in
      Alcotest.(check int) "second saguaro render exits 0" 0 second_result.code;
      Alcotest.(check string) "second saguaro render is quiet" ""
        second_result.stderr;
      Alcotest.(check (list (triple string int string)))
        "two saguaro renders are byte identical"
        (snapshot first) (snapshot second);
      let explained =
        run_agentctl
          [
            "explain";
            "--client";
            "saguaro";
            "--source";
            real_root;
            "--json";
          ]
      in
      Alcotest.(check int) "saguaro explain exits 0" 0 explained.code;
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
(* Suite                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia saguaro render"
    [
      ( "render",
        [
          Alcotest.test_case "real corpus" `Quick test_saguaro_real_corpus;
          Alcotest.test_case "golden without role" `Quick test_saguaro_golden;
        ] );
      ("release", [ Alcotest.test_case "deterministic artifacts" `Quick test_determinism ]);
      ( "integrity",
        [
          Alcotest.test_case "template drift is loud" `Quick
            test_template_drift_is_loud;
        ] );
      ("cli", [ Alcotest.test_case "render twice and explain" `Quick test_cli_saguaro ]);
    ]
