open Nomia

module C = Nomia.Canonical
module R = Nomia.Release
module S = Nomia.Source
module M = Nomia.Model

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
  Sys.readdir source |> Array.iter (fun name ->
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

let with_temp_fixture fixture action =
  let temporary = Filename.temp_dir "nomia-release-" "" in
  let root = Filename.concat temporary "source" in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree temporary with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () ->
      copy_tree fixture root;
      action root)

let replace_first ~needle ~replacement text =
  let needle_len = String.length needle in
  let text_len = String.length text in
  let rec loop index =
    if index + needle_len > text_len then
      Alcotest.failf "replace_first: %S not found" needle
    else if String.sub text index needle_len = needle then
      String.sub text 0 index ^ replacement
      ^ String.sub text (index + needle_len) (text_len - index - needle_len)
    else loop (index + 1)
  in
  loop 0

let load root =
  match S.load ~root with
  | Ok source -> source
  | Error diagnostics ->
      Alcotest.failf "load %s: %d diagnostics" root (List.length diagnostics)

let member (key : string) (json : Yojson.Safe.t) : Yojson.Safe.t option =
  match json with
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let json_string (value : Yojson.Safe.t) =
  match value with `String text -> text | _ -> ""

let json_int (value : Yojson.Safe.t) = match value with `Int number -> number | _ -> -1

let json_list (value : Yojson.Safe.t) =
  match value with `List items -> items | _ -> []

let member_list (key : string) (json : Yojson.Safe.t) =
  member key json |> Option.value ~default:(`List []) |> json_list

let rec json_keys (json : Yojson.Safe.t) =
  match json with
  | `Assoc entries ->
      List.concat_map (fun (key, value) -> key :: json_keys value) entries
  | `List values -> List.concat_map json_keys values
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _ -> []

let paths (json : Yojson.Safe.t list) =
  List.map
    (fun item ->
      member "path" item |> Option.value ~default:`Null |> json_string)
    json

let is_sorted values =
  values = List.sort String.compare values

let sha256_of_abc =
  "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

(* ------------------------------------------------------------------ *)
(* Canonical JSON                                                     *)
(* ------------------------------------------------------------------ *)

let test_sort_is_recursive_and_idempotent () =
  let value =
    `Assoc
      [
        ("b", `List [ `Assoc [ ("z", `Int 1); ("a", `Int 2) ] ]);
        ("a", `Assoc [ ("y", `Bool true); ("b", `Null) ]);
      ]
  in
  let sorted = C.sort value in
  (match sorted with
  | `Assoc [ ("a", `Assoc [ ("b", _); ("y", _) ]); ("b", `List [ `Assoc [ ("a", _); ("z", _) ] ]) ] ->
      Alcotest.(check bool) "nested object keys are sorted" true true
  | _ -> Alcotest.fail "sort did not produce the expected nested key order");
  Alcotest.(check bool) "sort is idempotent" true (C.sort sorted = sorted)

let test_equivalent_insertion_orders () =
  let left =
    `Assoc
      [
        ("object", `Assoc [ ("beta", `Int 2); ("alpha", `Int 1) ]);
        ("list", `List [ `Int 3; `Int 1; `Int 2 ]);
        ("text", `String "a\nb");
      ]
  in
  let right =
    `Assoc
      [
        ("text", `String "a\nb");
        ("list", `List [ `Int 3; `Int 1; `Int 2 ]);
        ("object", `Assoc [ ("alpha", `Int 1); ("beta", `Int 2) ]);
      ]
  in
  Alcotest.(check string) "equivalent maps serialize identically"
    (C.to_string left) (C.to_string right);
  Alcotest.(check string) "equivalent maps digest identically"
    (C.manifest_digest left) (C.manifest_digest right);
  Alcotest.(check bool) "lists keep author order" true
    (contains_sub (C.to_string left) "[3,1,2]")

let test_compact_output () =
  let text = C.to_string (`Assoc [ ("a", `Int 1); ("b", `List [ `Bool true ]) ]) in
  Alcotest.(check string) "compact separators" "{\"a\":1,\"b\":[true]}" text;
  Alcotest.(check string) "float rendering is stable" "1.5"
    (C.to_string (`Float 1.5));
  Alcotest.(check string) "integral floats keep a decimal point" "2.0"
    (C.to_string (`Float 2.0))

let test_non_finite_rejected () =
  let rejects label value =
    match C.to_string value with
    | _ -> Alcotest.failf "%s was accepted" label
    | exception Invalid_argument _ -> ()
  in
  rejects "NaN" (`Float Float.nan);
  rejects "infinity" (`Float Float.infinity);
  rejects "negative infinity" (`List [ `Float Float.neg_infinity ]);
  (match C.to_string (`Float 0.0) with
  | "0.0" -> ()
  | other -> Alcotest.failf "zero rendered as %S" other)

let test_length_prefix () =
  Alcotest.(check string) "length prefix layout"
    "\000\000\000\000\000\000\000\003abc"
    (C.length_prefixed "abc");
  Alcotest.(check bool) "single-segment framing is unambiguous" true
    (C.length_prefixed "ab" ^ "c" <> (C.length_prefixed "a" ^ "bc"));
  Alcotest.(check bool) "two-segment framing is unambiguous" true
    (C.length_prefixed "ab" ^ C.length_prefixed "c"
    <> (C.length_prefixed "a" ^ C.length_prefixed "bc"));
  Alcotest.(check bool) "empty frame is eight zero bytes" true
    (C.length_prefixed "" = String.make 8 '\000')

let test_manifest_digest_domain () =
  let value = `Assoc [ ("k", `String "v") ] in
  let digest = C.manifest_digest value in
  Alcotest.(check int) "digest has sha256 prefix and 64 hex digits" 71
    (String.length digest);
  Alcotest.(check string) "digest names sha256" "sha256:"
    (String.sub digest 0 7);
  Alcotest.(check bool) "digest is repeatable" true
    (digest = C.manifest_digest (C.sort value))

let test_sha256_helpers () =
  Alcotest.(check string) "sha256 abc" sha256_of_abc (C.sha256_hex "abc");
  let path = Filename.temp_file "nomia-sha-" ".txt" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      write_file path "abc";
      Alcotest.(check string) "sha256_file abc" sha256_of_abc
        (C.sha256_file path);
      Alcotest.(check int) "sha256_file is 64 hex digits" 64
        (String.length (C.sha256_file path)))

(* ------------------------------------------------------------------ *)
(* Manifest over the real corpus                                      *)
(* ------------------------------------------------------------------ *)

let test_real_corpus_is_deterministic () =
  let first = load ".." in
  let second = load ".." in
  let first_release = R.build first and second_release = R.build second in
  Alcotest.(check string) "source digest is stable"
    (R.source_digest first) (R.source_digest second);
  Alcotest.(check string) "release digest is stable"
    (R.digest first_release) (R.digest second_release);
  Alcotest.(check string) "manifest bytes are stable"
    (R.to_string first_release) (R.to_string second_release);
  Alcotest.(check bool) "source digest differs from release digest" true
    (R.source_digest first <> R.digest first_release)

let test_real_manifest_shape () =
  let source = load ".." in
  let release = R.build source in
  let manifest = R.manifest_json release in
  Alcotest.(check string) "manifest schema" "nomia-release/v1"
    (member "schema" manifest |> Option.value ~default:`Null |> json_string);
  Alcotest.(check string) "manifest compiler" source.policy.compiler
    (member "compiler" manifest |> Option.value ~default:`Null |> json_string);
  Alcotest.(check string) "manifest release" source.policy.release
    (member "release" manifest |> Option.value ~default:`Null |> json_string);
  Alcotest.(check string) "manifest digest matches accessor" (R.digest release)
    (member "digest" manifest |> Option.value ~default:`Null |> json_string);
  Alcotest.(check int) "manifest names every rule" (List.length source.rules)
    (List.length (member_list "rules" manifest));
  Alcotest.(check int) "manifest names every skill" (List.length source.skills)
    (List.length (member_list "skills" manifest));
  Alcotest.(check int) "manifest names every role" (List.length source.roles)
    (List.length (member_list "roles" manifest));
  Alcotest.(check int) "manifest names every adapter"
    (List.length source.adapters)
    (List.length (member_list "adapters" manifest));
  Alcotest.(check int) "manifest names every source file"
    (List.length source.files)
    (List.length (member_list "source_files" manifest));
  Alcotest.(check (list string)) "source file paths are sorted"
    (List.sort String.compare (List.map fst source.files))
    (paths (member_list "source_files" manifest));
  Alcotest.(check bool) "artifacts default to empty" true
    (member_list "artifacts" manifest = []);
  Alcotest.(check bool) "exceptions default to the loaded ids" true
    (member_list "exceptions" manifest = [])

let test_manifest_rules_are_complete () =
  let source = load ".." in
  let rule = List.hd source.rules in
  let manifest = R.manifest_json (R.build source) in
  let manifest_rule =
    member_list "rules" manifest
    |> List.find (fun item ->
           member "id" item |> Option.value ~default:`Null |> json_string
           = rule.id)
  in
  Alcotest.(check int) "rule revision" rule.revision
    (member "revision" manifest_rule |> Option.value ~default:`Null |> json_int);
  Alcotest.(check string) "rule owner" rule.owner
    (member "owner" manifest_rule |> Option.value ~default:`Null |> json_string);
  Alcotest.(check string) "rule body hash" (C.sha256_hex rule.body)
    (member "body_sha256" manifest_rule |> Option.value ~default:`Null
   |> json_string);
  Alcotest.(check string) "rule source path" rule.source
    (member "source" manifest_rule |> Option.value ~default:`Null |> json_string);
  Alcotest.(check bool) "rule carries enforcement" true
    (member "enforcement" manifest_rule <> Some `Null);
  Alcotest.(check bool) "rule carries guards as a list" true
    (match member "guards" manifest_rule with Some (`List _) -> true | _ -> false);
  Alcotest.(check bool) "rule carries supersedes (null or string)" true
    (match member "supersedes" manifest_rule with
    | Some `Null | Some (`String _) -> true
    | _ -> false)

let test_manifest_has_no_host_data_or_time () =
  let source = load ".." in
  let release = R.build source in
  let text = R.to_string release in
  Alcotest.(check bool) "manifest has no /home/ path" false
    (contains_sub text "/home/");
  Alcotest.(check bool) "manifest has no /Users/ path" false
    (contains_sub text "/Users/");
  Alcotest.(check bool) "manifest has no /tmp/ path" false
    (contains_sub text "/tmp/");
  let forbidden = [ "timestamp"; "generated_at"; "created_at"; "time"; "generated" ] in
  let keys = json_keys (R.manifest_json release) in
  Alcotest.(check bool) "manifest has no timestamp-like key" false
    (List.exists (fun key -> List.mem key forbidden) keys);
  let absolute_paths =
    let recorded =
      paths (member_list "source_files" (R.manifest_json release))
      @ (member_list "rules" (R.manifest_json release)
        |> List.map (fun item ->
               member "source" item |> Option.value ~default:`Null |> json_string))
      @ (member_list "roles" (R.manifest_json release)
        |> List.map (fun item ->
               member "source" item |> Option.value ~default:`Null |> json_string))
      @ (member_list "adapters" (R.manifest_json release)
        |> List.map (fun item ->
               member "source" item |> Option.value ~default:`Null |> json_string))
    in
    List.filter (fun path -> path <> "" && path.[0] = '/') recorded
  in
  Alcotest.(check (list string)) "every recorded source path is relative" []
    absolute_paths

(* ------------------------------------------------------------------ *)
(* Digest sensitivity                                                 *)
(* ------------------------------------------------------------------ *)

let test_rule_body_byte_changes_digest () =
  with_temp_fixture "fixtures/valid/minimal" (fun root ->
      let before = load root in
      let before_release = R.build before in
      let rule_path = Filename.concat root "rules/core.md" in
      write_file rule_path (read_file rule_path ^ "!");
      let after = load root in
      let after_release = R.build after in
      Alcotest.(check bool) "one body byte changes the source digest" true
        (R.source_digest before <> R.source_digest after);
      Alcotest.(check bool) "one body byte changes the release digest" true
        (R.digest before_release <> R.digest after_release);
      let body_hash release =
        member_list "rules" (R.manifest_json release)
        |> List.hd |> fun item ->
        member "body_sha256" item |> Option.value ~default:`Null |> json_string
      in
      Alcotest.(check bool) "one body byte changes the recorded body hash" true
        (body_hash before_release <> body_hash after_release))

let test_adapter_version_changes_digest () =
  let source = load ".." in
  let bumped =
    {
      source with
      adapters =
        List.map
          (fun (adapter : M.adapter) ->
            if adapter.id = "opencode" then
              { adapter with version = adapter.version + 1 }
            else adapter)
          source.adapters;
    }
  in
  Alcotest.(check bool) "adapter version in the model changes the digest" true
    (R.digest (R.build source) <> R.digest (R.build bumped));
  Alcotest.(check string) "the source digest ignores the selected adapter data"
    (R.source_digest source) (R.source_digest bumped);
  with_temp_fixture "fixtures/valid/minimal" (fun root ->
      let before = R.build (load root) in
      let adapter_path = Filename.concat root "adapters/opencode.yaml" in
      write_file adapter_path
        (replace_first ~needle:"version: 1" ~replacement:"version: 2"
           (read_file adapter_path));
      let after = R.build (load root) in
      Alcotest.(check bool) "adapter version changes the digest" true
        (R.digest before <> R.digest after);
      let version release =
        member_list "adapters" (R.manifest_json release)
        |> List.hd
        |> fun item ->
        member "version" item |> Option.value ~default:`Null |> json_int
      in
      Alcotest.(check int) "manifest records the new adapter version" 2
        (version after))

let test_selected_role_changes_digest () =
  let source = load ".." in
  let plain = R.build source in
  let reviewer = R.build ~role:"code-reviewer" source in
  let reviewer_again = R.build ~role:"code-reviewer" (load "..") in
  Alcotest.(check bool) "selected role changes the digest" true
    (R.digest plain <> R.digest reviewer);
  Alcotest.(check string) "same role selection is stable" (R.digest reviewer)
    (R.digest reviewer_again);
  Alcotest.(check string) "manifest records the selected role" "code-reviewer"
    (member "role" (R.manifest_json reviewer)
    |> Option.value ~default:`Null |> json_string);
  Alcotest.(check bool) "unselected role is null" true
    (member "role" (R.manifest_json plain) = Some `Null)

let fragment_manifest_digest manifest =
  match member "fragments" (R.manifest_json manifest) with
  | Some (`List (`Assoc fields :: _)) -> (
      match List.assoc_opt "digest" fields with
      | Some (`String digest) -> digest
      | _ -> "")
  | _ -> ""

let test_fragment_digest_changes_digest () =
  let source = load ".." in
  let plain = R.build source in
  let root = [ ("AGENTS.md", "sha256:aaa") ] in
  let one = R.build ~fragments:root source in
  let two = R.build ~fragments:[ ("AGENTS.md", "sha256:bbb") ] source in
  Alcotest.(check bool) "fragment digest changes the release digest" true
    (R.digest plain <> R.digest one);
  Alcotest.(check bool) "different fragment digests differ" true
    (R.digest one <> R.digest two);
  Alcotest.(check string) "manifest records the fragment digest" "sha256:bbb"
    (fragment_manifest_digest two);
  Alcotest.(check bool) "project identity changes the digest" true
    (R.digest (R.build ~project:"tuna" ~fragments:root source)
    <> R.digest
         (R.build ~project:"pp" ~fragments:root source))

let test_exception_ids_change_digest () =
  let source = load ".." in
  let plain = R.build source in
  let one = R.build ~exceptions:[ "exc.alpha" ] source in
  let two = R.build ~exceptions:[ "exc.beta" ] source in
  let reordered = R.build ~exceptions:[ "exc.beta"; "exc.alpha" ] source in
  let canonical_order = R.build ~exceptions:[ "exc.alpha"; "exc.beta" ] source in
  Alcotest.(check bool) "an exception id changes the digest" true
    (R.digest plain <> R.digest one);
  Alcotest.(check bool) "different exception ids differ" true
    (R.digest one <> R.digest two);
  Alcotest.(check string) "exception id order does not change the digest"
    (R.digest canonical_order) (R.digest reordered);
  Alcotest.(check (list string)) "manifest exception ids are sorted"
    [ "exc.alpha"; "exc.beta" ]
    (member_list "exceptions" (R.manifest_json reordered)
    |> List.map json_string)

let test_artifacts_do_not_change_input_digest () =
  let source = load ".." in
  let first =
    R.build ~artifacts:[ ("b.txt", "second"); ("a.txt", "first") ] source
  in
  let second =
    R.build ~artifacts:[ ("a.txt", "different"); ("b.txt", "bytes") ] source
  in
  Alcotest.(check string) "artifact content does not change the input digest"
    (R.digest first) (R.digest second);
  let manifest = R.manifest_json first in
  Alcotest.(check (list string)) "artifact paths are sorted" [ "a.txt"; "b.txt" ]
    (paths (member_list "artifacts" manifest));
  Alcotest.(check string) "artifact a is hashed" (C.sha256_hex "first")
    (member_list "artifacts" manifest
    |> List.find (fun item ->
           member "path" item |> Option.value ~default:`Null |> json_string
           = "a.txt")
    |> fun item ->
    member "sha256" item |> Option.value ~default:`Null |> json_string);
  let renamed = R.build ~artifacts:[ ("c.txt", "first") ] source in
  Alcotest.(check bool) "artifact paths still appear in the manifest" true
    (R.to_string first <> R.to_string renamed);
  let absolute_rejected =
    match
      R.build ~artifacts:[ ("/absolute", "x") ] source
    with
    | _ -> false
    | exception Invalid_argument _ -> true
  in
  Alcotest.(check bool) "absolute artifact paths are rejected" true
    absolute_rejected

let test_write_manifest () =
  let source = load ".." in
  let release = R.build source in
  let root = Filename.temp_dir "nomia-write-" "" in
  Fun.protect
    ~finally:(fun () -> try remove_tree root with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () ->
      let directory = Filename.concat root "nested/nomia" in
      match R.write_manifest ~dir:directory release with
      | Error message -> Alcotest.failf "write_manifest: %s" message
      | Ok () ->
          let path = Filename.concat directory "manifest.json" in
          Alcotest.(check bool) "manifest file exists" true (Sys.file_exists path);
          let written = read_file path in
          Alcotest.(check string) "written bytes are canonical plus newline"
            (R.to_string release ^ "\n") written)

(* ------------------------------------------------------------------ *)
(* Suite                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia_release"
    [
      ( "canonical",
        [
          Alcotest.test_case "sort is recursive and idempotent" `Quick
            test_sort_is_recursive_and_idempotent;
          Alcotest.test_case "equivalent insertion orders" `Quick
            test_equivalent_insertion_orders;
          Alcotest.test_case "compact output" `Quick test_compact_output;
          Alcotest.test_case "non-finite floats rejected" `Quick
            test_non_finite_rejected;
          Alcotest.test_case "length prefix" `Quick test_length_prefix;
          Alcotest.test_case "manifest digest domain" `Quick
            test_manifest_digest_domain;
          Alcotest.test_case "sha256 helpers" `Quick test_sha256_helpers;
        ] );
      ( "manifest",
        [
          Alcotest.test_case "real corpus is deterministic" `Quick
            test_real_corpus_is_deterministic;
          Alcotest.test_case "real manifest shape" `Quick
            test_real_manifest_shape;
          Alcotest.test_case "rules are complete" `Quick
            test_manifest_rules_are_complete;
          Alcotest.test_case "no host data or time" `Quick
            test_manifest_has_no_host_data_or_time;
          Alcotest.test_case "write manifest" `Quick test_write_manifest;
        ] );
      ( "digest sensitivity",
        [
          Alcotest.test_case "rule body byte" `Quick
            test_rule_body_byte_changes_digest;
          Alcotest.test_case "adapter version" `Quick
            test_adapter_version_changes_digest;
          Alcotest.test_case "selected role" `Quick
            test_selected_role_changes_digest;
          Alcotest.test_case "fragment digest" `Quick
            test_fragment_digest_changes_digest;
          Alcotest.test_case "exception ids" `Quick
            test_exception_ids_change_digest;
          Alcotest.test_case "artifact independence" `Quick
            test_artifacts_do_not_change_input_digest;
        ] );
    ]
