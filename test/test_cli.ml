open Nomia

(* ------------------------------------------------------------------ *)
(* Process helpers                                                     *)
(* ------------------------------------------------------------------ *)

let start_dir = Sys.getcwd ()
let binary = Filename.concat start_dir "../bin/agentctl.exe"

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
  let temporary = Filename.temp_dir "nomia-cli-" "" in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree temporary with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () -> action temporary)

type process_result = { code : int; stdout : string; stderr : string }

let spawn args =
  let out_path = Filename.temp_file "agentctl-stdout-" ".log" in
  let err_path = Filename.temp_file "agentctl-stderr-" ".log" in
  let out_fd =
    Unix.openfile out_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let err_fd =
    Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let argv = Array.of_list (binary :: args) in
  let pid =
    try Unix.create_process binary argv Unix.stdin out_fd err_fd
    with error ->
      Unix.close out_fd;
      Unix.close err_fd;
      raise error
  in
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

let run ?cwd args =
  let previous = Sys.getcwd () in
  Fun.protect
    ~finally:(fun () -> Sys.chdir previous)
    (fun () ->
      (match cwd with Some dir -> Sys.chdir dir | None -> ());
      spawn args)

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

let jstring = function `String value -> value | _ -> ""

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

(* ------------------------------------------------------------------ *)
(* File snapshots                                                      *)
(* ------------------------------------------------------------------ *)

let rec walk_files root relative =
  let directory =
    if relative = "" then root else Filename.concat root relative
  in
  Sys.readdir directory |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
         let child = if relative = "" then name else relative ^ "/" ^ name in
         let full = Filename.concat root child in
         if Sys.is_directory full then walk_files root child
         else [ child ])

let snapshot root =
  walk_files root ""
  |> List.map (fun relative ->
         let full = Filename.concat root relative in
         (relative, (Unix.stat full).st_perm land 0o777, read_file full))

(* ------------------------------------------------------------------ *)
(* check                                                               *)
(* ------------------------------------------------------------------ *)

let test_check_real_corpus () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  let result = run [ "check"; ".." ] in
  Alcotest.(check int) "valid corpus exits 0" 0 result.code;
  Alcotest.(check string) "valid corpus is quiet" "" result.stdout;
  Alcotest.(check string) "valid corpus has no stderr" "" result.stderr;
  let json = run [ "check"; ".."; "--json" ] in
  Alcotest.(check int) "valid corpus json exits 0" 0 json.code;
  let parsed = Yojson.Safe.from_string json.stdout in
  Alcotest.(check bool) "json ok" true
    (member "ok" parsed = Some (`Bool true));
  Alcotest.(check bool) "json diagnostics empty" true
    (member "diagnostics" parsed = Some (`List []))

let test_check_invalid_fixtures () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  let cases =
    [
      ("duplicate-id", "rule.duplicate-id");
      ("missing-owner", "rule.owner.missing");
      ("broken-skill-reference", "skill.rules.target");
      ("unauthorized-override", "role.relaxes.final");
      ("expired-exception", "exception.expires");
    ]
  in
  List.iter
    (fun (fixture, code) ->
      let result =
        run [ "check"; Filename.concat "fixtures/invalid" fixture ]
      in
      Alcotest.(check int) (fixture ^ " exits 1") 1 result.code;
      Alcotest.(check bool) (fixture ^ " names " ^ code) true
        (contains_sub result.stdout code);
      let json =
        run
          [
            "check";
            Filename.concat "fixtures/invalid" fixture;
            "--json";
          ]
      in
      Alcotest.(check int) (fixture ^ " json exits 1") 1 json.code;
      let parsed = Yojson.Safe.from_string json.stdout in
      Alcotest.(check bool) (fixture ^ " json ok is false") true
        (member "ok" parsed = Some (`Bool false));
      Alcotest.(check bool) (fixture ^ " json names code") true
        (contains_sub json.stdout code))
    cases

let test_missing_source () =
  Unix.putenv "NOMIA_SOURCE" "";
  with_temp_dir (fun temporary ->
      let result = run ~cwd:temporary [ "check" ] in
      Alcotest.(check int) "missing source exits 1" 1 result.code;
      let output = result.stdout ^ result.stderr in
      Alcotest.(check bool) "actionable message" true
        (contains_sub output "no policy source found");
      Alcotest.(check bool) "no stack trace" false
        (contains_sub output "Fatal error"
        || contains_sub output "Stack overflow"))

(* ------------------------------------------------------------------ *)
(* render and explain                                                  *)
(* ------------------------------------------------------------------ *)

let render_args ~client ~role ~out =
  [
    "render";
    "--client";
    client;
    "--role";
    role;
    "--source";
    "..";
    "--out";
    out;
  ]

let test_render_twice_identical () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  with_temp_dir (fun temporary ->
      let first = Filename.concat temporary "first" in
      let second = Filename.concat temporary "second" in
      Unix.mkdir first 0o755;
      Unix.mkdir second 0o755;
      let first_result =
        run (render_args ~client:"opencode" ~role:"implementer" ~out:first)
      in
      Alcotest.(check int) "first render exits 0" 0 first_result.code;
      Alcotest.(check string) "first render quiet" "" first_result.stderr;
      let second_result =
        run (render_args ~client:"opencode" ~role:"implementer" ~out:second)
      in
      Alcotest.(check int) "second render exits 0" 0 second_result.code;
      Alcotest.(check string) "second render quiet" "" second_result.stderr;
      Alcotest.(check (list (triple string int string)))
        "two renders are byte identical"
        (snapshot first) (snapshot second);
      let manifest_path =
        Filename.concat first ".nomia/manifest.json"
      in
      Alcotest.(check bool) "manifest written last" true
        (Sys.file_exists manifest_path);
      Alcotest.(check bool) "explain written" true
        (Sys.file_exists (Filename.concat first ".nomia/explain.json"));
      let manifest = Yojson.Safe.from_string (read_file manifest_path) in
      let listed =
        match member "artifacts" manifest with
        | Some (`List entries) -> entries
        | _ -> []
      in
      Alcotest.(check bool) "manifest lists the rendered artifacts" true
        (List.length listed > 0);
      List.iter
        (fun entry ->
          let path =
            member "path" entry |> Option.value ~default:`Null |> jstring
          in
          let expected =
            member "sha256" entry |> Option.value ~default:`Null |> jstring
          in
          Alcotest.(check bool) "manifest excludes itself" false
            (path = ".nomia/manifest.json");
          Alcotest.(check string) ("manifest hash for " ^ path) expected
            (Canonical.sha256_hex
               (read_file (Filename.concat first path))))
        listed)

let test_render_refuses_nonempty () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  with_temp_dir (fun temporary ->
      let out = Filename.concat temporary "out" in
      Unix.mkdir out 0o755;
      let sentinel = Filename.concat out "sentinel" in
      write_file sentinel "keep";
      let result =
        run (render_args ~client:"generic-agents" ~role:"implementer" ~out)
      in
      Alcotest.(check int) "non-empty out exits 1" 1 result.code;
      Alcotest.(check bool) "message names the non-empty directory" true
        (contains_sub result.stderr "not empty");
      Alcotest.(check string) "sentinel untouched" "keep" (read_file sentinel);
      Alcotest.(check bool) "no AGENTS.md written" false
        (Sys.file_exists (Filename.concat out "AGENTS.md")))

let test_explain_json_matches_render () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  with_temp_dir (fun temporary ->
      let out = Filename.concat temporary "out" in
      Unix.mkdir out 0o755;
      let rendered =
        run (render_args ~client:"generic-agents" ~role:"implementer" ~out)
      in
      Alcotest.(check int) "render exits 0" 0 rendered.code;
      let explained =
        run
          [
            "explain";
            "--client";
            "generic-agents";
            "--role";
            "implementer";
            "--source";
            "..";
            "--json";
          ]
      in
      Alcotest.(check int) "explain exits 0" 0 explained.code;
      Alcotest.(check string) "explain json equals render explain"
        (read_file (Filename.concat out ".nomia/explain.json"))
        explained.stdout;
      let text =
        run
          [
            "explain";
            "--client";
            "generic-agents";
            "--role";
            "implementer";
            "--source";
            "..";
          ]
      in
      Alcotest.(check int) "text explain exits 0" 0 text.code;
      Alcotest.(check bool) "text explain names release" true
        (contains_sub text.stdout "release: 0.1.0"))

let test_bad_selection_leaves_out_absent () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  with_temp_dir (fun temporary ->
      let bad_role_out = Filename.concat temporary "bad-role" in
      let bad_role =
        run
          [
            "render";
            "--client";
            "opencode";
            "--role";
            "not-a-role";
            "--source";
            "..";
            "--out";
            bad_role_out;
          ]
      in
      Alcotest.(check int) "unknown role exits 1" 1 bad_role.code;
      Alcotest.(check bool) "unknown role is diagnosed" true
        (contains_sub bad_role.stderr "compose.role.unknown");
      Alcotest.(check bool) "unknown role wrote nothing" false
        (Sys.file_exists bad_role_out);
      let bad_client_out = Filename.concat temporary "bad-client" in
      let bad_client =
        run
          [
            "render";
            "--client";
            "not-a-client";
            "--source";
            "..";
            "--out";
            bad_client_out;
          ]
      in
      Alcotest.(check int) "unknown client exits 1" 1 bad_client.code;
      Alcotest.(check bool) "unknown client is diagnosed" true
        (contains_sub bad_client.stderr "compose.client.unknown");
      Alcotest.(check bool) "unknown client wrote nothing" false
        (Sys.file_exists bad_client_out))

let test_no_absolute_paths () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  with_temp_dir (fun temporary ->
      let out = Filename.concat temporary "out" in
      Unix.mkdir out 0o755;
      let result =
        run (render_args ~client:"opencode" ~role:"implementer" ~out)
      in
      Alcotest.(check int) "render exits 0" 0 result.code;
      walk_files out ""
      |> List.iter (fun relative ->
             let contents = read_file (Filename.concat out relative) in
             Alcotest.(check bool) (relative ^ " has no absolute path") false
               (has_absolute_path contents)))

(* ------------------------------------------------------------------ *)
(* version                                                             *)
(* ------------------------------------------------------------------ *)

let test_version_without_source () =
  Unix.putenv "NOMIA_SOURCE" "";
  with_temp_dir (fun temporary ->
      let result = run ~cwd:temporary [ "version" ] in
      Alcotest.(check int) "version exits 0" 0 result.code;
      Alcotest.(check string) "version falls back to dev" "agentctl dev\n"
        result.stdout)

(* ------------------------------------------------------------------ *)
(* Suite                                                               *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia cli"
    [
      ( "check",
        [
          Alcotest.test_case "real corpus" `Quick test_check_real_corpus;
          Alcotest.test_case "five invalid fixtures" `Quick
            test_check_invalid_fixtures;
          Alcotest.test_case "missing source" `Quick test_missing_source;
        ] );
      ( "render",
        [
          Alcotest.test_case "two identical renders" `Quick
            test_render_twice_identical;
          Alcotest.test_case "non-empty out" `Quick test_render_refuses_nonempty;
          Alcotest.test_case "bad role or client" `Quick
            test_bad_selection_leaves_out_absent;
          Alcotest.test_case "no absolute paths" `Quick test_no_absolute_paths;
        ] );
      ( "explain",
        [ Alcotest.test_case "json equals render" `Quick
            test_explain_json_matches_render ] );
      ("version", [ Alcotest.test_case "no source" `Quick test_version_without_source ]);
    ]
