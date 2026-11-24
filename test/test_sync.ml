open Nomia

module S = Nomia.Store
module Sy = Nomia.Sync

(* ------------------------------------------------------------------ *)
(* Process and filesystem helpers                                      *)
(* ------------------------------------------------------------------ *)

let binary =
  Filename.concat
    (Filename.dirname (Unix.realpath Sys.executable_name))
    "../bin/agentctl.exe"

let real_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root
    when Sys.file_exists (Filename.concat root "nomia/policy.yaml") ->
      Filename.concat root "nomia"
  | _ -> ".."

let today = "2030-01-01"

let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let rec mkdir_p path =
  if path = "" || path = "." || path = "/" then ()
  else if Sys.file_exists path then ()
  else begin
    mkdir_p (Filename.dirname path);
    Unix.mkdir path 0o755
  end

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | exception Sys_error _ -> ()
  | stats ->
      if stats.Unix.st_kind = Unix.S_DIR then begin
        Sys.readdir path
        |> Array.iter (fun name -> remove_tree (Filename.concat path name));
        Unix.rmdir path
      end
      else Unix.unlink path

let rec copy_path source destination =
  match Unix.lstat source with
  | stats when stats.Unix.st_kind = Unix.S_DIR ->
      if not (Sys.file_exists destination) then Unix.mkdir destination 0o755;
      Sys.readdir source
      |> Array.iter (fun name ->
             copy_path (Filename.concat source name)
               (Filename.concat destination name))
  | _ ->
      mkdir_p (Filename.dirname destination);
      write_file destination (read_file source)

let copy_source destination =
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

let has_absolute_path text =
  contains_sub text "/home/" || contains_sub text "/Users/"
  || contains_sub text "/private/" || contains_sub text "/tmp/"

type process_result = { code : int; stdout : string; stderr : string }

let env_key entry =
  match String.index_opt entry '=' with
  | Some index -> String.sub entry 0 index
  | None -> entry

let spawn ~env args =
  let out_path = Filename.temp_file "agentctl-sync-out-" ".log" in
  let err_path = Filename.temp_file "agentctl-sync-err-" ".log" in
  let out_fd =
    Unix.openfile out_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let err_fd =
    Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let argv = Array.of_list (binary :: args) in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close out_fd;
        Unix.close err_fd)
      (fun () ->
        Unix.create_process_env binary argv env Unix.stdin out_fd err_fd)
  in
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

type workspace = {
  root : string;
  home : string;
  config : string;
  cache : string;
  src : string;
  repo : string;
  render : string;
}

let with_workspace action =
  let root = Filename.temp_dir "nomia-sync-" "" in
  let previous_cache = Sys.getenv_opt "XDG_CACHE_HOME" in
  Fun.protect
    ~finally:(fun () ->
      (match previous_cache with
      | Some value -> Unix.putenv "XDG_CACHE_HOME" value
      | None -> Unix.putenv "XDG_CACHE_HOME" "");
      try remove_tree root with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () ->
      let home = Filename.concat root "home" in
      let config = Filename.concat root "config" in
      let cache = Filename.concat root "cache" in
      let src = Filename.concat root "source" in
      let repo = Filename.concat root "repo" in
      let render = Filename.concat root "render" in
      List.iter (fun path -> mkdir_p path)
        [ home; config; cache; src; repo ];
      Unix.putenv "XDG_CACHE_HOME" cache;
      copy_source src;
      action { root; home; config; cache; src; repo; render })

let workspace_env workspace ?(extra = []) () =
  let overrides =
    [
      "HOME=" ^ workspace.home;
      "XDG_CONFIG_HOME=" ^ workspace.config;
      "XDG_CACHE_HOME=" ^ workspace.cache;
      "NOMIA_TODAY=" ^ today;
    ]
    @ extra
  in
  let keys =
    List.map env_key overrides
    @ [ "NOMIA_SOURCE"; "NOMIA_FAULT" ]
  in
  let base =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry -> not (List.mem (env_key entry) keys))
  in
  let merged =
    List.fold_left
      (fun acc entry ->
        let key = env_key entry in
        List.filter (fun existing -> env_key existing <> key) acc @ [ entry ])
      [] overrides
  in
  Array.of_list (base @ merged)

let run workspace ?(extra = []) args =
  spawn ~env:(workspace_env workspace ~extra ()) args

let sync_args workspace ?repo ?project ?(inherit_root = false) ?client
    ?(update_pin = false) ~source () =
  [ "sync" ]
  @ (match repo with Some repo -> [ "--repo"; repo ] | None -> [])
  @ (match project with Some project -> [ "--project"; project ] | None -> [])
  @ (if inherit_root then [ "--inherit-root" ] else [])
  @ (match client with Some client -> [ "--client"; client ] | None -> [])
  @ [ "--source"; source ]
  @ (if update_pin then [ "--update-pin" ] else [])

let doctor_args workspace ?repo ?project ?(inherit_root = false) ?client ?source
    () =
  [ "doctor" ]
  @ (match repo with Some repo -> [ "--repo"; repo ] | None -> [])
  @ (match project with Some project -> [ "--project"; project ] | None -> [])
  @ (if inherit_root then [ "--inherit-root" ] else [])
  @ (match client with Some client -> [ "--client"; client ] | None -> [])
  @ (match source with Some source -> [ "--source"; source ] | None -> [])
  @ [ "--json" ]

let run_sync workspace ?repo ?project ?(inherit_root = false) ?client
    ?(update_pin = false) ?(source = workspace.src) ?(extra = []) () =
  run workspace ~extra
    (sync_args workspace ?repo ?project ~inherit_root ?client ~update_pin
       ~source ())

let run_doctor workspace ?repo ?project ?(inherit_root = false) ?client ?source
    ?(extra = []) () =
  let source = match source with Some source -> Some source | None -> Some workspace.src in
  run workspace ~extra
    (doctor_args workspace ?repo ?project ~inherit_root ?client ?source ())

(* ------------------------------------------------------------------ *)
(* JSON and snapshot helpers                                           *)
(* ------------------------------------------------------------------ *)

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let json_string key json =
  match member key json with
  | Some (`String value) -> value
  | _ -> Alcotest.failf "missing string field %s" key

let json_string_opt key json =
  match member key json with
  | Some (`String value) -> Some value
  | _ -> None

let json_int key json =
  match member key json with
  | Some (`Int value) -> value
  | _ -> Alcotest.failf "missing int field %s" key

let json_strings key json =
  match member key json with
  | Some (`List values) ->
      List.filter_map (function `String value -> Some value | _ -> None) values
  | _ -> Alcotest.failf "missing string list field %s" key

let parse_json text = Yojson.Safe.from_string text

let state_path target = Filename.concat target ".nomia/state.json"
let lock_path target = Filename.concat target "nomia.lock"
let current_path target = Filename.concat target ".nomia/current"

let state_json target = parse_json (read_file (state_path target))

let installed_digest target =
  match S.current_digest (S.target_of_root target) with
  | Ok (Some digest) -> digest
  | _ -> Alcotest.failf "no managed current in %s" target

let release_root target digest =
  Filename.concat target (".nomia/releases/" ^ digest)

let rec walk_files root relative =
  let directory =
    if relative = "" then root else Filename.concat root relative
  in
  Sys.readdir directory |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
         let child = if relative = "" then name else relative ^ "/" ^ name in
         let full = Filename.concat root child in
         if Sys.is_directory full then walk_files root child
         else
           let stats = Unix.lstat full in
           [ (child, stats.Unix.st_perm land 0o777, read_file full) ])

let snapshot root = walk_files root ""

let read_lock target = read_file (lock_path target)

let write_full_coverage workspace =
  let adapter =
    {|id: opencode
version: 1
description: >
  A test adapter whose capability matrix covers every control used by the
  seeded rules, so doctor can report a fully current installation.
instructions:
  file: AGENTS.md
  discovery:
    - AGENTS.md
  includes: true
skills:
  dir: .opencode/skills
  auto_discovery: true
  explicit_reference: true
roles: true
mcp: true
hooks: true
permissions: true
atomic_output: true
controls:
  advisory:
    - instruction
    - skill
  gate:
    - ci-verification
    - ci-review
    - evidence-verifier
  consent:
    - host-hook
    - permissions
  prohibition:
    - filesystem
    - permissions
unsupported:
  - none
|}
  in
  write_file (Filename.concat workspace.src "adapters/opencode.yaml") adapter

let set_lock_source_digest target digest =
  let lines = String.split_on_char '\n' (read_lock target) in
  let lines =
    List.map
      (fun line ->
        if starts_with ~prefix:"source_digest:" line then
          "source_digest: " ^ digest
        else line)
      lines
  in
  write_file (lock_path target) (String.concat "\n" lines)

let assert_no_absolute_paths name text =
  Alcotest.(check bool) (name ^ " has no /home/") false
    (contains_sub text "/home/");
  Alcotest.(check bool) (name ^ " has no /Users/") false
    (contains_sub text "/Users/");
  Alcotest.(check bool) (name ^ " has no /private/") false
    (contains_sub text "/private/");
  Alcotest.(check bool) (name ^ " has no /tmp/") false
    (contains_sub text "/tmp/")

let assert_no_timestamp_keys name text =
  let lower = String.lowercase_ascii text in
  List.iter
    (fun key ->
      Alcotest.(check bool) (Printf.sprintf "%s has no %s key" name key) false
        (contains_sub lower ("\"" ^ key ^ "\"")))
    [ "timestamp"; "created_at"; "updated_at"; "mtime"; "time" ]

let check_snapshot name expected actual =
  Alcotest.(check bool) name true (expected = actual)

(* ------------------------------------------------------------------ *)
(* First sync: layout, lock, state, entrypoints, render identity       *)
(* ------------------------------------------------------------------ *)

let test_first_sync_layout () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      let result = run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true () in
      Alcotest.(check int) "first sync exits 0" 0 result.code;
      Alcotest.(check bool) "state.json exists" true
        (Sys.file_exists (state_path workspace.repo));
      Alcotest.(check bool) "lock exists" true
        (Sys.file_exists (lock_path workspace.repo));
      let state = state_json workspace.repo in
      Alcotest.(check string) "state schema" "nomia-state/v1"
        (json_string "schema" state);
      Alcotest.(check string) "state client" "opencode"
        (json_string "client" state);
      Alcotest.(check string) "state release" "0.2.0"
        (json_string "release" state);
      let digest = json_string "digest" state in
      Alcotest.(check bool) "state digest is sha256" true
        (starts_with ~prefix:"sha256:" digest);
      let entrypoints = json_strings "entrypoints" state in
      Alcotest.(check (list string)) "entrypoints sorted"
        [ ".opencode/skills"; "AGENTS.md"; "opencode.json" ]
        entrypoints;
      Alcotest.(check string) "current selects state digest"
        ("releases/" ^ digest) (Unix.readlink (current_path workspace.repo));
      List.iter
        (fun entrypoint ->
          Alcotest.(check bool)
            (Printf.sprintf "entrypoint %s exists" entrypoint)
            true
            (Sys.file_exists (Filename.concat workspace.repo entrypoint));
          Alcotest.(check bool)
            (Printf.sprintf "entrypoint %s is a symlink" entrypoint)
            true
            (match Unix.lstat (Filename.concat workspace.repo entrypoint) with
            | stats -> stats.Unix.st_kind = Unix.S_LNK
            | exception Unix.Unix_error _ -> false))
        entrypoints;
      let agents_link = Unix.readlink (Filename.concat workspace.repo "AGENTS.md") in
      Alcotest.(check string) "AGENTS.md stable link"
        ".nomia/current/AGENTS.md" agents_link;
      let skills_link =
        Unix.readlink (Filename.concat workspace.repo ".opencode/skills")
      in
      Alcotest.(check string) "skills stable link"
        "../.nomia/current/.opencode/skills" skills_link;
      let lock = read_lock workspace.repo in
      Alcotest.(check bool) "lock names schema" true
        (contains_sub lock "schema: nomia-lock/v2");
      Alcotest.(check bool) "lock names the source digest" true
        (contains_sub lock
           ("source_digest: " ^ json_string "source_digest" state));
      Alcotest.(check bool) "lock names opencode" true
        (contains_sub lock "client: opencode");
      let manifest =
        parse_json
          (read_file
             (Filename.concat (release_root workspace.repo digest)
                ".nomia/manifest.json"))
      in
      Alcotest.(check string) "manifest digest equals state digest" digest
        (json_string "digest" manifest);
      let release = snapshot (release_root workspace.repo digest) in
      Alcotest.(check bool) "release has files" true (release <> []);
      let render = run workspace
          [ "render"; "--client"; "opencode"; "--source"; workspace.src;
            "--out"; workspace.render ]
      in
      Alcotest.(check int) "render exits 0" 0 render.code;
      let rendered = snapshot workspace.render in
      check_snapshot "managed release bytes equal agentctl render" rendered
        release;
      let state_text = read_file (state_path workspace.repo) in
      let manifest_text =
        read_file
          (Filename.concat (release_root workspace.repo digest)
             ".nomia/manifest.json")
      in
      assert_no_absolute_paths "state" state_text;
      assert_no_absolute_paths "manifest" manifest_text)

(* ------------------------------------------------------------------ *)
(* Idempotent second sync and doctor current                           *)
(* ------------------------------------------------------------------ *)

let test_second_sync_idempotent () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      let first =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "first sync exits 0" 0 first.code;
      let digest = installed_digest workspace.repo in
      let state_before = read_file (state_path workspace.repo) in
      let lock_before = read_lock workspace.repo in
      let release_before = snapshot (release_root workspace.repo digest) in
      let second =
        run_sync workspace ~repo:workspace.repo ~client:"opencode" ()
      in
      Alcotest.(check int) "second sync exits 0" 0 second.code;
      Alcotest.(check string) "current digest unchanged" digest
        (installed_digest workspace.repo);
      Alcotest.(check bool) "state bytes unchanged" true
        (state_before = read_file (state_path workspace.repo));
      Alcotest.(check bool) "lock bytes unchanged" true
        (lock_before = read_lock workspace.repo);
      check_snapshot "release bytes unchanged" release_before
        (snapshot (release_root workspace.repo digest));
      let doctor = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor current exits 0" 0 doctor.code;
      let report = parse_json doctor.stdout in
      Alcotest.(check string) "doctor state current" "current"
        (json_string "state" report);
      Alcotest.(check string) "doctor installed digest" digest
        (json_string "digest" (Option.get (member "installed" report))))

(* ------------------------------------------------------------------ *)
(* Source change: stale, then current after re-pin                     *)
(* ------------------------------------------------------------------ *)

let test_source_change_stale_then_current () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      let first =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "first sync exits 0" 0 first.code;
      let old_digest = installed_digest workspace.repo in
      let rule_path = Filename.concat workspace.src "rules/engineering.scope.md" in
      let original_rule = read_file rule_path in
      write_file rule_path
        (original_rule ^ "\n\nAdditional verification note for the sync test.\n");
      let before = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor stale exits 1" 1 before.code;
      Alcotest.(check string) "doctor state stale" "stale"
        (json_string "state" (parse_json before.stdout));
      let refused =
        run_sync workspace ~repo:workspace.repo ~client:"opencode" ()
      in
      Alcotest.(check int) "sync without re-pin refuses" 1 refused.code;
      Alcotest.(check string) "current unchanged after refusal" old_digest
        (installed_digest workspace.repo);
      let updated =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "sync with re-pin exits 0" 0 updated.code;
      let new_digest = installed_digest workspace.repo in
      Alcotest.(check bool) "current digest changed" true
        (new_digest <> old_digest);
      Alcotest.(check bool) "old release kept for rollback" true
        (Sys.file_exists (release_root workspace.repo old_digest));
      let after = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor current after sync exits 0" 0 after.code;
      Alcotest.(check string) "doctor state current after sync" "current"
        (json_string "state" (parse_json after.stdout));
      Alcotest.(check bool) "new digest cached" true
        (List.mem new_digest (S.cache_digests ())))

(* ------------------------------------------------------------------ *)
(* Test-only fault before the current swap                             *)
(* ------------------------------------------------------------------ *)

let test_fault_before_swap () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      let first =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "first sync exits 0" 0 first.code;
      let old_digest = installed_digest workspace.repo in
      let agents_path = Filename.concat workspace.repo "AGENTS.md" in
      let old_agents = read_file agents_path in
      let releases_before =
        S.list_dir (Filename.concat workspace.repo ".nomia/releases")
      in
      let rule_path = Filename.concat workspace.src "rules/engineering.scope.md" in
      write_file rule_path
        (read_file rule_path ^ "\n\nFault injection change.\n");
      let failed =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ~extra:[ "NOMIA_FAULT=before-swap" ] ()
      in
      Alcotest.(check int) "fault sync exits 1" 1 failed.code;
      Alcotest.(check string) "current unchanged after fault" old_digest
        (installed_digest workspace.repo);
      Alcotest.(check bool) "entrypoint bytes unchanged after fault" true
        (old_agents = read_file agents_path);
      Alcotest.(check (list string)) "no partial release visible"
        releases_before
        (S.list_dir (Filename.concat workspace.repo ".nomia/releases"));
      let recovered =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "recovery sync exits 0" 0 recovered.code;
      Alcotest.(check bool) "recovery advances current" true
        (installed_digest workspace.repo <> old_digest))

(* ------------------------------------------------------------------ *)
(* Unmanaged and hand-authored targets                                 *)
(* ------------------------------------------------------------------ *)

let test_unmanaged_conflict () =
  with_workspace (fun workspace ->
      let agents = Filename.concat workspace.repo "AGENTS.md" in
      write_file agents "# Hand-authored repository instructions\n";
      let result =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "sync refuses unmanaged AGENTS.md" 1 result.code;
      Alcotest.(check bool) "AGENTS.md untouched" true
        (read_file agents = "# Hand-authored repository instructions\n");
      Alcotest.(check bool) "no lock written" false
        (Sys.file_exists (lock_path workspace.repo));
      Alcotest.(check bool) "no managed root created" false
        (Sys.file_exists (Filename.concat workspace.repo ".nomia"));
      Alcotest.(check bool) "message suggests a fragment" true
        (contains_sub result.stderr "AGENTS.fragment.md");
      let doctor = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor unmanaged exits 1" 1 doctor.code;
      Alcotest.(check string) "doctor state unmanaged" "unmanaged"
        (json_string "state" (parse_json doctor.stdout)))

let test_hand_edited_release () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      let first =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "first sync exits 0" 0 first.code;
      let digest = installed_digest workspace.repo in
      let agents =
        Filename.concat (release_root workspace.repo digest) "AGENTS.md"
      in
      write_file agents (read_file agents ^ "\nhand edit\n");
      let doctor = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor hand-edited exits 1" 1 doctor.code;
      let report = parse_json doctor.stdout in
      Alcotest.(check string) "doctor state hand-edited" "hand-edited"
        (json_string "state" report);
      Alcotest.(check bool) "doctor reports the changed file" true
        (contains_sub doctor.stdout "AGENTS.md");
      let refused =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "sync refuses hand-edited release" 1 refused.code;
      Alcotest.(check bool) "hand edit untouched" true
        (contains_sub (read_file agents) "hand edit"))

let test_missing_target () =
  with_workspace (fun workspace ->
      let doctor = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor missing exits 1" 1 doctor.code;
      Alcotest.(check string) "doctor state missing" "missing"
        (json_string "state" (parse_json doctor.stdout));
      Alcotest.(check bool) "doctor creates nothing" false
        (Sys.file_exists (Filename.concat workspace.repo ".nomia")))

(* ------------------------------------------------------------------ *)
(* Lock mismatch and the reviewable re-pin                             *)
(* ------------------------------------------------------------------ *)

let test_lock_mismatch_and_repin () =
  with_workspace (fun workspace ->
      let no_lock =
        run_sync workspace ~repo:workspace.repo ~client:"opencode" ()
      in
      Alcotest.(check int) "sync without a lock refuses" 1 no_lock.code;
      Alcotest.(check bool) "refused sync writes no lock" false
        (Sys.file_exists (lock_path workspace.repo));
      write_full_coverage workspace;
      let first =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "first sync exits 0" 0 first.code;
      let digest = installed_digest workspace.repo in
      let release_before = snapshot (release_root workspace.repo digest) in
      let current_before = Unix.readlink (current_path workspace.repo) in
      let rule_path = Filename.concat workspace.src "rules/engineering.scope.md" in
      let original_rule = read_file rule_path in
      write_file rule_path (original_rule ^ "\n\nUnpinned change.\n");
      let refused =
        run_sync workspace ~repo:workspace.repo ~client:"opencode" ()
      in
      Alcotest.(check int) "lock source mismatch refuses" 1 refused.code;
      Alcotest.(check string) "mismatch leaves current untouched" current_before
        (Unix.readlink (current_path workspace.repo));
      write_file rule_path original_rule;
      let source_digest = json_string "source_digest" (state_json workspace.repo) in
      let fake = "sha256:" ^ String.make 64 '0' in
      set_lock_source_digest workspace.repo fake;
      let repinned =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "re-pin exits 0" 0 repinned.code;
      Alcotest.(check bool) "re-pin corrects the lock source digest" true
        (contains_sub (read_lock workspace.repo)
           ("source_digest: " ^ source_digest));
      Alcotest.(check bool) "re-pin leaves the lock free of the fake digest"
        false
        (contains_sub (read_lock workspace.repo) fake);
      Alcotest.(check string) "re-pin leaves current untouched" current_before
        (Unix.readlink (current_path workspace.repo));
      check_snapshot "re-pin rewrites only the lock" release_before
        (snapshot (release_root workspace.repo digest)))

(* ------------------------------------------------------------------ *)
(* User-level client sync                                              *)
(* ------------------------------------------------------------------ *)

let test_client_sync () =
  with_workspace (fun workspace ->
      let result =
        run_sync workspace ~client:"opencode" ()
      in
      Alcotest.(check int) "client sync exits 0" 0 result.code;
      let target = Filename.concat workspace.config "nomia/clients/opencode" in
      Alcotest.(check bool) "client target state exists" true
        (Sys.file_exists (state_path target));
      Alcotest.(check bool) "client target has no lock" false
        (Sys.file_exists (lock_path target));
      let digest = installed_digest target in
      let state = state_json target in
      Alcotest.(check string) "client state client" "opencode"
        (json_string "client" state);
      Alcotest.(check (list string)) "client entrypoints"
        [ ".opencode/skills"; "AGENTS.md"; "opencode.json" ]
        (json_strings "entrypoints" state);
      List.iter
        (fun entrypoint ->
          let path = Filename.concat target entrypoint in
          let link = Unix.readlink path in
          Alcotest.(check bool)
            (Printf.sprintf "%s resolves into current" entrypoint)
            true
            (contains_sub link ".nomia/current/");
          Alcotest.(check bool)
            (Printf.sprintf "%s is readable" entrypoint)
            true (Sys.file_exists path);
          Alcotest.(check bool)
            (Printf.sprintf "%s resolves into the selected release" entrypoint)
            true
            (starts_with ~prefix:(release_root target digest)
               (Unix.realpath path)))
        [ "AGENTS.md"; "opencode.json"; ".opencode/skills" ];
      Alcotest.(check bool) "no native config at HOME root" false
        (Sys.file_exists (Filename.concat workspace.home "AGENTS.md")
        || Sys.file_exists (Filename.concat workspace.home "opencode.json"));
      Alcotest.(check int) "HOME stays empty" 0
        (List.length (walk_files workspace.home ""));
      Alcotest.(check bool) "no native config at config root" false
        (Sys.file_exists
           (Filename.concat workspace.config "nomia/AGENTS.md"));
      let config = read_file (Filename.concat target "opencode.json") in
      Alcotest.(check bool) "opencode config keeps ask" true
        (contains_sub config "\"effect\": \"ask\"");
      Alcotest.(check bool) "opencode config keeps deny" true
        (contains_sub config "\"effect\": \"deny\"");
      Alcotest.(check bool) "credential deny remains" true
        (contains_sub config "*.aws/credentials");
      Alcotest.(check bool) "nested SSH key deny remains" true
        (contains_sub config "*id_rsa*");
      Alcotest.(check bool) "git push ask remains" true
        (contains_sub config "git push *");
      let doctor = run_doctor workspace ~client:"opencode" () in
      Alcotest.(check int) "client doctor exits 0" 0 doctor.code;
      Alcotest.(check string) "client doctor is partial" "partial"
        (json_string "state" (parse_json doctor.stdout));
      Alcotest.(check bool) "cache has the client digest" true
        (List.mem digest (S.cache_digests ()));
      assert_no_absolute_paths "client state" (read_file (state_path target));
      assert_no_absolute_paths "doctor" doctor.stdout)

(* ------------------------------------------------------------------ *)
(* Offline last-known-good and cache loading                           *)
(* ------------------------------------------------------------------ *)

let test_offline_cache () =
  with_workspace (fun workspace ->
      let result = run_sync workspace ~client:"opencode" () in
      Alcotest.(check int) "client sync exits 0" 0 result.code;
      let target = Filename.concat workspace.config "nomia/clients/opencode" in
      let digest = installed_digest target in
      let agents = Filename.concat target "AGENTS.md" in
      let agents_before = read_file agents in
      Alcotest.(check bool) "installed policy is non-empty" true
        (String.length agents_before > 0);
      (match S.load_cached ~digest with
      | Error message -> Alcotest.failf "cached release did not load: %s" message
      | Ok cached ->
          Alcotest.(check bool) "cached manifest digest matches" true
            (json_string "digest" cached.S.manifest = digest);
          Alcotest.(check bool) "cached explain is non-empty" true
            (cached.S.explain <> `Null);
          Alcotest.(check bool) "cached artifacts are non-empty" true
            (cached.S.artifacts <> []));
      let moved = workspace.src ^ "-offline" in
      Sys.rename workspace.src moved;
      let doctor =
        run_doctor workspace ~client:"opencode" ~source:(workspace.src) ()
      in
      Alcotest.(check int) "offline doctor exits 0" 0 doctor.code;
      let report = parse_json doctor.stdout in
      Alcotest.(check string) "offline state" "offline-last-known-good"
        (json_string "state" report);
      Alcotest.(check bool) "offline warning is explicit" true
        (match json_string_opt "warning" report with
        | Some warning -> String.length warning > 0
        | None -> false);
      Alcotest.(check bool) "offline lists cached digest" true
        (List.mem digest
           (json_strings "digests" (Option.get (member "cached" report))));
      Alcotest.(check string) "offline keeps installed digest" digest
        (json_string "digest" (Option.get (member "installed" report)));
      Alcotest.(check bool) "offline does not rewrite the policy" true
        (agents_before = read_file agents);
      let empty_cache = Filename.concat workspace.root "empty-cache" in
      mkdir_p empty_cache;
      let missing =
        run_doctor workspace ~client:"generic-agents" ~source:workspace.src
          ~extra:[ "XDG_CACHE_HOME=" ^ empty_cache ] ()
      in
      Alcotest.(check int) "unavailable source without cache exits 2" 2
        missing.code;
      Alcotest.(check string) "no-cache state" "source-unavailable"
        (json_string "state" (parse_json missing.stdout)))

(* ------------------------------------------------------------------ *)
(* Partial enforcement coverage is reported, not drift                *)
(* ------------------------------------------------------------------ *)

let test_partial_coverage () =
  with_workspace (fun workspace ->
      let result = run_sync workspace ~client:"opencode" () in
      Alcotest.(check int) "client sync exits 0" 0 result.code;
      let target = Filename.concat workspace.config "nomia/clients/opencode" in
      let digest_before = installed_digest target in
      let doctor = run_doctor workspace ~client:"opencode" () in
      Alcotest.(check int) "partial doctor exits 0" 0 doctor.code;
      let report = parse_json doctor.stdout in
      Alcotest.(check string) "partial state" "partial"
        (json_string "state" report);
      let coverage = Option.get (member "enforcement" report) in
      Alcotest.(check bool) "coverage counts enforcement rules" true
        (json_int "rules" coverage > 0);
      Alcotest.(check bool) "coverage reports partial rules" true
        (json_int "partial" coverage > 0);
      Alcotest.(check bool) "coverage reports unsupported rules" true
        (json_int "unsupported" coverage > 0);
      Alcotest.(check bool) "coverage names host-hook" true
        (List.mem "host-hook" (json_strings "unsupported_controls" coverage));
      Alcotest.(check bool) "coverage names operations.authority" true
        (List.mem "operations.authority"
           (json_strings "unsupported_rules" coverage));
      Alcotest.(check string) "partial is not drift" digest_before
        (installed_digest target))

(* ------------------------------------------------------------------ *)
(* JSON hygiene                                                       *)
(* ------------------------------------------------------------------ *)

let test_json_hygiene () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      let result =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "sync exits 0" 0 result.code;
      let digest = installed_digest workspace.repo in
      let state_text = read_file (state_path workspace.repo) in
      let manifest_text =
        read_file
          (Filename.concat (release_root workspace.repo digest)
             ".nomia/manifest.json")
      in
      let doctor = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor exits 0" 0 doctor.code;
      List.iter
        (fun (name, text) ->
          assert_no_absolute_paths name text;
          assert_no_timestamp_keys name text;
          Alcotest.(check bool) (name ^ " has no workspace root") false
            (contains_sub text workspace.root))
        [ ("state", state_text); ("manifest", manifest_text);
          ("doctor", doctor.stdout) ])


let test_per_client_cache () =
  with_workspace (fun workspace ->
      let first = run_sync workspace ~client:"generic-agents" () in
      Alcotest.(check int) "generic sync exits 0" 0 first.code;
      let second = run_sync workspace ~client:"opencode" () in
      Alcotest.(check int) "opencode sync exits 0" 0 second.code;
      let digests = S.cache_digests () in
      Alcotest.(check int) "two client releases are cached separately" 2
        (List.length digests);
      let clients =
        List.filter_map
          (fun digest ->
            match S.load_cached ~digest with
            | Error _ -> None
            | Ok cached ->
                Some
                  (match member "client" cached.S.manifest with
                  | Some (`String client) -> client
                  | _ -> ""))
          digests
      in
      Alcotest.(check (list string)) "cache clients are distinct"
        [ "generic-agents"; "opencode" ]
        (List.sort String.compare clients))

let test_erde_skill_entrypoint () =
  with_workspace (fun workspace ->
      let synced =
        run_sync workspace ~repo:workspace.repo ~client:"erde" ~update_pin:true ()
      in
      Alcotest.(check int) "erde sync exits 0" 0 synced.code;
      let target = S.target_of_root workspace.repo in
      let digest =
        match S.current_digest target with
        | Ok (Some digest) -> digest
        | _ -> Alcotest.fail "erde sync did not select a release"
      in
      let state =
        match S.read_state target with
        | Ok (Some state) -> state
        | _ -> Alcotest.fail "erde state is missing"
      in
      Alcotest.(check bool) "erde skills is an entrypoint" true
        (List.mem "skills" state.S.entrypoints);
      let skills = Filename.concat workspace.repo "skills" in
      Alcotest.(check bool) "erde skills entrypoint exists" true
        (Sys.file_exists skills);
      Alcotest.(check bool) "erde skills resolves into the release" true
        (starts_with ~prefix:(release_root workspace.repo digest)
           (Unix.realpath skills)))

let test_fault_after_swap_recovers () =
  with_workspace (fun workspace ->
      let first = run_sync workspace ~repo:workspace.repo ~update_pin:true () in
      Alcotest.(check int) "first sync exits 0" 0 first.code;
      let second_src = workspace.src ^ "-after-swap" in
      copy_path workspace.src second_src;
      let rule = Filename.concat second_src "rules/engineering.scope.md" in
      write_file rule (read_file rule ^ "\nInjected reviewed line.\n");
      let failed =
        run_sync workspace ~repo:workspace.repo ~update_pin:true
          ~source:second_src ~extra:[ "NOMIA_FAULT=after-swap" ] ()
      in
      Alcotest.(check bool) "after-swap fault is reported" true (failed.code <> 0);
      let doctor = run_doctor workspace ~repo:workspace.repo ~source:second_src () in
      Alcotest.(check int) "doctor reports the un-re-pinned lock as stale" 1
        doctor.code;
      Alcotest.(check string) "the complete new current is not hand-edited"
        "stale"
        (json_string "state" (Yojson.Safe.from_string doctor.stdout));
      let target = S.target_of_root workspace.repo in
      let recovered =
        run_sync workspace ~repo:workspace.repo ~update_pin:true ~source:second_src ()
      in
      Alcotest.(check int) "next sync recovers the staged state" 0 recovered.code;
      Alcotest.(check bool) "state.next is gone" false
        (S.file_exists (S.state_next_path target)))


let test_first_sync_after_swap_recovers () =
  with_workspace (fun workspace ->
      let target = S.target_of_root workspace.repo in
      let failed =
        run_sync workspace ~repo:workspace.repo ~update_pin:true
          ~extra:[ "NOMIA_FAULT=after-swap" ] ()
      in
      Alcotest.(check bool) "first-sync after-swap fault is reported" true
        (failed.code <> 0);
      Alcotest.(check bool) "current names a complete release" true
        (match S.current_digest target with Ok (Some _) -> true | _ -> false);
      Alcotest.(check bool) "staged state survives" true
        (S.file_exists (S.state_next_path target));
      let doctor = run_doctor workspace ~repo:workspace.repo () in
      Alcotest.(check int) "doctor accepts the recoverable target" 0 doctor.code;
      let recovered = run_sync workspace ~repo:workspace.repo ~update_pin:true () in
      Alcotest.(check int) "next sync repairs entrypoints" 0 recovered.code;
      Alcotest.(check bool) "durable state is promoted" true
        (S.file_exists target.S.state_path);
      Alcotest.(check bool) "staged state is removed" false
        (S.file_exists (S.state_next_path target));
      Alcotest.(check bool) "AGENTS entrypoint is created" true
        (S.file_exists (Filename.concat workspace.repo "AGENTS.md")))


let test_saguaro_skill_entrypoint () =
  with_workspace (fun workspace ->
      let repo = workspace.repo ^ "-saguaro" in
      mkdir_p repo;
      let synced = run_sync workspace ~repo ~client:"saguaro" ~update_pin:true () in
      Alcotest.(check int) "saguaro sync exits 0" 0 synced.code;
      let target = S.target_of_root repo in
      let state =
        match S.read_state target with
        | Ok (Some state) -> state
        | _ -> Alcotest.fail "saguaro state is missing"
      in
      Alcotest.(check bool) "saguaro skills is an entrypoint" true
        (List.mem "skills" state.S.entrypoints);
      Alcotest.(check bool) "saguaro skills entrypoint exists" true
        (Sys.file_exists (Filename.concat repo "skills")))

let test_project_sync_uses_root_pin () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      write_file (Filename.concat workspace.repo "AGENTS.fragment.md")
        "Repository-wide instructions for every project.\n";
      let project_root = Filename.concat workspace.repo "tuna" in
      let sibling_root = Filename.concat workspace.repo "pp" in
      mkdir_p project_root;
      mkdir_p sibling_root;
      write_file (Filename.concat project_root "AGENTS.fragment.md")
        "Tuna-specific instructions only.\n";
      write_file (Filename.concat sibling_root "AGENTS.fragment.md")
        "Pricklypear-specific instructions only.\n";
      let synced =
        run_sync workspace ~repo:workspace.repo ~project:"tuna"
          ~client:"opencode" ~update_pin:true ()
      in
      Alcotest.(check int) "project sync exits 0" 0 synced.code;
      Alcotest.(check bool) "one root pin is written" true
        (Sys.file_exists (lock_path workspace.repo));
      Alcotest.(check bool) "no project-local lock is written" false
        (Sys.file_exists (lock_path project_root));
      Alcotest.(check bool) "project target is installed" true
        (Sys.file_exists (state_path project_root));
      Alcotest.(check bool) "root target is not installed" false
        (Sys.file_exists (state_path workspace.repo));
      let agents = read_file (Filename.concat project_root "AGENTS.md") in
      Alcotest.(check bool) "root fragment is included" true
        (contains_sub agents "Repository-wide instructions for every project.");
      Alcotest.(check bool) "project fragment is included" true
        (contains_sub agents "Tuna-specific instructions only.");
      let doctor =
        run_doctor workspace ~repo:workspace.repo ~project:"tuna" ()
      in
      Alcotest.(check int) "project doctor exits 0" 0 doctor.code;
      let report = parse_json doctor.stdout in
      Alcotest.(check string) "doctor state current" "current"
        (json_string "state" report);
      Alcotest.(check string) "doctor records the project" "tuna"
        (json_string "project" report);
      let sibling =
        run_sync workspace ~repo:workspace.repo ~project:"pp"
          ~client:"generic-agents" ()
      in
      Alcotest.(check int)
        "one root pin also serves another project and client" 0 sibling.code;
      Alcotest.(check bool) "sibling project installs under itself" true
        (Sys.file_exists (state_path sibling_root));
      Alcotest.(check bool) "sibling does not receive tuna text" false
        (contains_sub
           (read_file (Filename.concat sibling_root "AGENTS.md"))
           "Tuna-specific instructions only."))

let test_project_overlay_sync () =
  with_workspace (fun workspace ->
      write_full_coverage workspace;
      write_file (Filename.concat workspace.repo "AGENTS.fragment.md")
        "Repository-wide instructions for every project.\n";
      let project_root = Filename.concat workspace.repo "tuna" in
      mkdir_p project_root;
      write_file (Filename.concat project_root "AGENTS.fragment.md")
        "Tuna-specific instructions only.\n";
      let root =
        run_sync workspace ~repo:workspace.repo ~client:"opencode"
          ~update_pin:true ()
      in
      Alcotest.(check int) "root sync exits 0" 0 root.code;
      let overlay =
        run_sync workspace ~repo:workspace.repo ~project:"tuna"
          ~inherit_root:true ~client:"opencode" ()
      in
      Alcotest.(check int) "overlay project sync exits 0" 0 overlay.code;
      Alcotest.(check bool) "root target remains installed" true
        (Sys.file_exists (state_path workspace.repo));
      Alcotest.(check bool) "project overlay target is installed" true
        (Sys.file_exists (state_path project_root));
      Alcotest.(check bool) "overlay has no project-local lock" false
        (Sys.file_exists (lock_path project_root));
      Alcotest.(check bool) "overlay has no duplicate skills" false
        (Sys.file_exists (Filename.concat project_root ".opencode"));
      let agents = read_file (Filename.concat project_root "AGENTS.md") in
      Alcotest.(check bool) "overlay excludes organization core" false
        (contains_sub agents "# Organization agent policy");
      Alcotest.(check bool) "overlay excludes root instructions" false
        (contains_sub agents "Repository-wide instructions for every project.");
      Alcotest.(check bool) "overlay includes project instructions" true
        (contains_sub agents "Tuna-specific instructions only.");
      let doctor =
        run_doctor workspace ~repo:workspace.repo ~project:"tuna"
          ~inherit_root:true ()
      in
      Alcotest.(check int) "overlay doctor exits 0" 0 doctor.code;
      let report = parse_json doctor.stdout in
      Alcotest.(check string) "overlay doctor is current" "current"
        (json_string "state" report);
      Alcotest.(check bool) "overlay doctor records inheritance" true
        (match member "inherit_root" report with
        | Some (`Bool true) -> true
        | _ -> false);
      let missing_root =
        let other = workspace.repo ^ "-no-root" in
        mkdir_p (Filename.concat other "tuna");
        write_file
          (Filename.concat (Filename.concat other "tuna") "AGENTS.fragment.md")
          "Project without a managed root.\n";
        run_sync workspace ~repo:other ~project:"tuna" ~inherit_root:true
          ~update_pin:true ()
      in
      Alcotest.(check bool) "overlay refuses without a managed root" true
        (missing_root.code <> 0))

(* ------------------------------------------------------------------ *)
(* Suite                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia-sync"
    [
      ( "sync",
        [
          Alcotest.test_case "first sync layout" `Quick
            test_first_sync_layout;
          Alcotest.test_case "second sync idempotent" `Quick
            test_second_sync_idempotent;
          Alcotest.test_case "source change stale then current" `Quick
            test_source_change_stale_then_current;
          Alcotest.test_case "fault before swap" `Quick test_fault_before_swap;
          Alcotest.test_case "unmanaged target" `Quick test_unmanaged_conflict;
          Alcotest.test_case "hand-edited release" `Quick
            test_hand_edited_release;
          Alcotest.test_case "missing target" `Quick test_missing_target;
          Alcotest.test_case "lock mismatch and re-pin" `Quick
            test_lock_mismatch_and_repin;
          Alcotest.test_case "project sync uses the root pin" `Quick
            test_project_sync_uses_root_pin;
          Alcotest.test_case "project overlay inherits the root" `Quick
            test_project_overlay_sync;
        ] );
      ( "distribution",
        [
          Alcotest.test_case "user client sync" `Quick test_client_sync;
          Alcotest.test_case "offline cache" `Quick test_offline_cache;
          Alcotest.test_case "partial coverage" `Quick test_partial_coverage;
          Alcotest.test_case "JSON hygiene" `Quick test_json_hygiene;
          Alcotest.test_case "per-client cache" `Quick test_per_client_cache;
          Alcotest.test_case "erde skills entrypoint" `Quick
            test_erde_skill_entrypoint;
          Alcotest.test_case "saguaro skills entrypoint" `Quick
            test_saguaro_skill_entrypoint;
          Alcotest.test_case "after-swap fault recovery" `Quick
            test_fault_after_swap_recovers;
          Alcotest.test_case "first-sync after-swap recovery" `Quick
            test_first_sync_after_swap_recovers;
        ] );
    ]
