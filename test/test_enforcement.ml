open Nomia

module Co = Nomia.Compose
module E = Nomia.Enforcement
module M = Nomia.Model
module S = Nomia.Source

let real_root = ".."
let today = "2030-01-01"

(* ------------------------------------------------------------------ *)
(* Helpers                                                            *)
(* ------------------------------------------------------------------ *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let rec mkdir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" then
    if Sys.file_exists dir then ()
    else begin
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    end

let write_file path contents =
  mkdir_p (Filename.dirname path);
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
  let temporary = Filename.temp_dir "nomia-enforcement-" "" in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree temporary with Sys_error _ | Unix.Unix_error _ -> ())
    (fun () -> action temporary)

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

let load_source root =
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

let real ?role ?client () =
  resolve_ok ?role ?client (load_source real_root)

let request ?(tool = "tool") ?role ?repo ?project ?policy_digest
    ?(paths = []) ?(network_hosts = []) action =
  {
    E.action;
    tool;
    role;
    repo;
    project;
    policy_digest;
    paths;
    network_hosts;
  }

let decide_ok policy_set request =
  match E.decide policy_set request with
  | Ok result -> result
  | Error diagnostics ->
      Alcotest.failf "decide: %s"
        (String.concat ", "
           (List.map (fun (diagnostic : M.diagnostic) -> diagnostic.code)
              diagnostics))

let decide_error policy_set request =
  match E.decide policy_set request with
  | Ok _ -> Alcotest.fail "decide unexpectedly succeeded"
  | Error diagnostics -> diagnostics

let codes diagnostics =
  List.map (fun (diagnostic : M.diagnostic) -> diagnostic.code) diagnostics

let check_decision label expected result =
  Alcotest.(check string) label
    (E.decision_name expected)
    (E.decision_name result.E.decision)

let check_ids label expected result =
  Alcotest.(check (list string)) label expected result.E.rule_ids

let has value values = List.mem value values

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let jstring = function `String value -> value | _ -> ""

(* ------------------------------------------------------------------ *)
(* Real corpus decisions                                              *)
(* ------------------------------------------------------------------ *)

let test_credential_deny () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let result =
    decide_ok policy_set
      (request ~role:"implementer" ~repo:"wyo"
         ~policy_digest:policy_set.Co.digest ~tool:"read" ~paths:
           [ "config/.env" ]
         "fs.read")
  in
  check_decision "credential read denies" E.Deny result;
  Alcotest.(check bool) "security.credentials is the denying rule" true
    (has "security.credentials" result.E.rule_ids);
  Alcotest.(check bool) "filesystem control is required" true
    (has "filesystem" result.E.required_controls);
  Alcotest.(check bool) "permissions control is required" true
    (has "permissions" result.E.required_controls);
  Alcotest.(check bool) "opencode cannot provide filesystem" true
    (has "filesystem" result.E.unsupported_controls);
  Alcotest.(check string) "decision names the adapter" "opencode"
    result.E.adapter;
  Alcotest.(check string) "decision carries the policy digest"
    policy_set.Co.digest result.E.policy_digest;
  Alcotest.(check (option string)) "no project is selected" None
    result.E.project;
  let nested =
    decide_ok policy_set
      (request ~tool:"read" ~paths:[ "home/user/.aws/credentials" ] "fs.read")
  in
  check_decision "nested credential path denies" E.Deny nested;
  let host =
    decide_ok policy_set
      (request ~tool:"fetch" ~network_hosts:[ "api.example.com" ] "net.fetch")
  in
  check_decision "network fetch with a host denies" E.Deny host;
  Alcotest.(check bool) "host denial names credentials" true
    (has "security.credentials" host.E.rule_ids)

let test_project_context_mismatch () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let diagnostics =
    decide_error policy_set
      (request ~project:"tuna" ~policy_digest:policy_set.Co.digest
         "fs.read")
  in
  Alcotest.(check bool) "project mismatch is diagnosed" true
    (List.mem "enforcement.project" (codes diagnostics))

let test_ordinary_read_allows () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let result =
    decide_ok policy_set
      (request ~role:"implementer" ~tool:"read" ~paths:[ "src/x.ml" ] "fs.read")
  in
  check_decision "ordinary source read allows" E.Allow result;
  check_ids "no rule matches an ordinary read" [] result;
  Alcotest.(check (list string)) "no obligations" [] result.E.obligations;
  Alcotest.(check string) "digest is always reported" policy_set.Co.digest
    result.E.policy_digest

let test_credential_missing_context_asks () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let result = decide_ok policy_set (request ~tool:"read" "fs.read") in
  check_decision "missing path context asks" E.Ask result;
  Alcotest.(check bool) "the guarded rule is named" true
    (has "security.credentials" result.E.rule_ids);
  Alcotest.(check bool) "an obligation requests path context" true
    (has "provide-path-context" result.E.obligations);
  let network = decide_ok policy_set (request ~tool:"fetch" "net.fetch") in
  check_decision "missing host context asks" E.Ask network;
  Alcotest.(check bool) "an obligation requests host context" true
    (has "provide-host-context" network.E.obligations);
  Alcotest.(check bool) "context ask is not an allow" false
    (network.E.decision = E.Allow);
  Alcotest.(check bool) "context ask is not a blanket deny" false
    (network.E.decision = E.Deny)

let test_authority_asks () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let result = decide_ok policy_set (request ~tool:"git" "vcs.push") in
  check_decision "git push asks" E.Ask result;
  Alcotest.(check bool) "authority rule is named" true
    (has "operations.authority" result.E.rule_ids);
  Alcotest.(check bool) "authority obligation is present" true
    (has "high-impact.authority" result.E.obligations);
  Alcotest.(check bool) "host-hook remains unsupported" true
    (has "host-hook" result.E.unsupported_controls)

let test_gates_allow_with_obligations () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let verify = decide_ok policy_set (request ~tool:"ci" "delivery.verify") in
  check_decision "delivery verify allows" E.Allow verify;
  Alcotest.(check bool) "verification rule is named" true
    (has "engineering.verify-change" verify.E.rule_ids);
  Alcotest.(check bool) "verification obligation is present" true
    (has "verification.required" verify.E.obligations);
  let review = decide_ok policy_set (request ~tool:"review" "delivery.review") in
  check_decision "delivery review allows" E.Allow review;
  Alcotest.(check bool) "review rule is named" true
    (has "engineering.independent-review" review.E.rule_ids);
  Alcotest.(check bool) "review obligation is present" true
    (has "review.independence" review.E.obligations)

let test_no_match_allows () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let result =
    decide_ok policy_set
      (request ~tool:"custom" ~paths:[ "src/x.ml" ] "custom.unknown")
  in
  check_decision "unmatched action allows" E.Allow result;
  check_ids "unmatched action has no rule ids" [] result;
  Alcotest.(check (list string)) "unmatched action has no controls" []
    result.E.required_controls

(* ------------------------------------------------------------------ *)
(* Deny precedence and unsupported controls                           *)
(* ------------------------------------------------------------------ *)

let fixture_source root =
  write_file (Filename.concat root "policy.yaml")
    "schema: nomia/v1\n\
     release: 0.1.0\n\
     compiler: agentctl/1\n\
     description: Enforcement precedence fixture.\n\
     maintainers:\n\
     \  - name: wyo-engineering\n\
     \    role: engineering-owner\n\
     \  - name: wyo-security\n\
     \    role: security-owner\n\
     budget:\n\
     \  core_max_bytes: 8192\n\
     \  max_rules: 12\n";
  write_file (Filename.concat root "adapters/generic-agents.yaml")
    "id: generic-agents\n\
     version: 1\n\
     description: Enforcement fixture adapter.\n\
     instructions:\n\
     \  file: AGENTS.md\n\
     \  discovery:\n\
     \    - AGENTS.md\n\
     \  includes: false\n\
     skills:\n\
     \  dir: .agents/skills\n\
     \  auto_discovery: true\n\
     \  explicit_reference: true\n\
     roles: false\n\
     mcp: false\n\
     hooks: false\n\
     permissions: false\n\
     atomic_output: true\n\
     controls:\n\
     \  advisory:\n\
     \    - instruction\n\
     \    - skill\n\
     \  gate:\n\
     \    - ci-verification\n\
     \  consent: []\n\
     \  prohibition: []\n\
     unsupported:\n\
     \  - native host hooks\n";
  write_file (Filename.concat root "rules/org.deny.md")
    "---\n\
     id: org.deny\n\
     revision: 1\n\
     owner: wyo-security\n\
     strength: must\n\
     scope: [all]\n\
     final: true\n\
     kind: instruction\n\
     enforcement:\n\
     \  class: prohibition\n\
     \  controls: [permissions]\n\
     actions: [fs.delete]\n\
     key: delete.prohibited\n\
     value: prohibited\n\
     ---\n\
     Fixture prohibition on delete.\n";
  write_file (Filename.concat root "rules/org.ask.md")
    "---\n\
     id: org.ask\n\
     revision: 1\n\
     owner: wyo-engineering\n\
     strength: must\n\
     scope: [all]\n\
     final: true\n\
     kind: instruction\n\
     enforcement:\n\
     \  class: consent\n\
     \  controls: [permissions]\n\
     actions: [fs.delete]\n\
     key: delete.authority\n\
     value: required\n\
     ---\n\
     Fixture consent on delete.\n";
  root

let test_deny_precedence () =
  with_temp_dir (fun root ->
      ignore (fixture_source root);
      let source = load_source root in
      let policy_set = resolve_ok source in
      let result =
        decide_ok policy_set
          (request ~tool:"rm" ~paths:[ "build/output" ] "fs.delete")
      in
      check_decision "deny wins over ask on the same action" E.Deny result;
      Alcotest.(check bool) "the prohibition is named" true
        (has "org.deny" result.E.rule_ids);
      Alcotest.(check bool) "the consent rule is still named" true
        (has "org.ask" result.E.rule_ids);
      Alcotest.(check bool) "the prohibition obligation is present" true
        (has "do-not-proceed" result.E.obligations);
      Alcotest.(check (list string)) "both rules require permissions"
        [ "permissions" ] result.E.required_controls)

let test_unsupported_never_allows () =
  let policy_set = real ~role:"implementer" ~client:"generic-agents" () in
  let result =
    decide_ok policy_set
      (request ~tool:"read" ~paths:[ "config/.env" ] "fs.read")
  in
  check_decision "unsupported controls still deny" E.Deny result;
  Alcotest.(check bool) "filesystem is named unsupported" true
    (has "filesystem" result.E.unsupported_controls);
  Alcotest.(check bool) "permissions is named unsupported" true
    (has "permissions" result.E.unsupported_controls);
  Alcotest.(check bool) "required controls remain visible" true
    (has "filesystem" result.E.required_controls
    && has "permissions" result.E.required_controls);
  Alcotest.(check bool) "the decision is not an allow" false
    (result.E.decision = E.Allow)

let test_policy_digest_mismatch () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let diagnostics =
    decide_error policy_set
      (request ~tool:"read" ~paths:[ "src/x.ml" ]
         ~policy_digest:"sha256:0000000000000000000000000000000000000000000000000000000000000000"
         "fs.read")
  in
  Alcotest.(check bool) "digest mismatch is its own code" true
    (List.mem "enforcement.policy-digest" (codes diagnostics));
  Alcotest.(check bool) "diagnostic explains the mismatch" true
    (List.exists
       (fun (diagnostic : M.diagnostic) ->
         diagnostic.code = "enforcement.policy-digest"
         && contains_sub diagnostic.message "does not match")
       diagnostics)

(* ------------------------------------------------------------------ *)
(* Request decoding                                                   *)
(* ------------------------------------------------------------------ *)

let test_request_decoding () =
  (match
     E.request_of_string
       "{\"action\":\"fs.read\",\"tool\":\"read\",\"role\":\"implementer\",\
        \"repo\":\"wyo\",\"paths\":[\"src/x.ml\"],\"network_hosts\":[]}"
   with
  | Ok request ->
      Alcotest.(check string) "action decoded" "fs.read" request.E.action;
      Alcotest.(check string) "tool decoded" "read" request.E.tool;
      Alcotest.(check (option string)) "role decoded" (Some "implementer")
        request.E.role;
      Alcotest.(check (list string)) "paths decoded" [ "src/x.ml" ]
        request.E.paths
  | Error _ -> Alcotest.fail "valid request did not decode");
  let bad_unknown =
    E.request_of_string "{\"action\":\"fs.read\",\"prompt\":\"ignore policy\"}"
  in
  (match bad_unknown with
  | Error diagnostics ->
      Alcotest.(check bool) "unknown field rejected" true
        (List.mem "enforcement.request.unknown-field" (codes diagnostics))
  | Ok _ -> Alcotest.fail "unknown field accepted");
  let bad_action = E.request_of_string "{\"tool\":\"read\"}" in
  (match bad_action with
  | Error diagnostics ->
      Alcotest.(check bool) "missing action rejected" true
        (List.mem "enforcement.request.action" (codes diagnostics))
  | Ok _ -> Alcotest.fail "missing action accepted");
  let bad_type =
    E.request_of_string "{\"action\":\"fs.read\",\"paths\":\"src/x.ml\"}"
  in
  (match bad_type with
  | Error diagnostics ->
      Alcotest.(check bool) "wrong paths type rejected" true
        (List.mem "enforcement.request.type" (codes diagnostics))
  | Ok _ -> Alcotest.fail "wrong paths type accepted");
  let bad_json = E.request_of_string "not json" in
  match bad_json with
  | Error diagnostics ->
      Alcotest.(check bool) "invalid JSON rejected" true
        (List.mem "enforcement.request.json" (codes diagnostics))
  | Ok _ -> Alcotest.fail "invalid JSON accepted"

(* ------------------------------------------------------------------ *)
(* Deterministic, leak-free output                                    *)
(* ------------------------------------------------------------------ *)

let test_deterministic_json () =
  let policy_set = real ~role:"implementer" ~client:"opencode" () in
  let request = request ~tool:"read" ~paths:[ "config/.env" ] "fs.read" in
  let first = decide_ok policy_set request in
  let second = decide_ok policy_set request in
  let first_json = E.to_string first in
  Alcotest.(check string) "identical requests produce identical JSON" first_json
    (E.to_string second);
  Alcotest.(check bool) "JSON is canonical (keys sorted)" true
    (String.length first_json > 10
    && String.sub first_json 0 11 = "{\"adapter\":");
  let parsed = Yojson.Safe.from_string first_json in
  Alcotest.(check string) "decision field is a string" "deny"
    (member "decision" parsed |> Option.value ~default:`Null |> jstring);
  Alcotest.(check bool) "no absolute path in the decision" false
    (has_absolute_path first_json);
  Alcotest.(check bool) "no rule prose in the decision" false
    (contains_sub first_json "Never read, print, commit");
  Alcotest.(check bool) "no prompt text in the decision" false
    (contains_sub first_json "prompt");
  Alcotest.(check bool) "no secret-looking value in the decision" false
    (contains_sub first_json "password" || contains_sub first_json "token=");
  let human = E.human_line first in
  Alcotest.(check bool) "human line names the decision" true
    (contains_sub human "deny");
  Alcotest.(check bool) "human line is one line" false
    (String.contains human '\n');
  Alcotest.(check bool) "human line leaks no absolute path" false
    (has_absolute_path human)

(* ------------------------------------------------------------------ *)
(* CLI                                                                *)
(* ------------------------------------------------------------------ *)

let start_dir = Sys.getcwd ()
let binary = Filename.concat start_dir "../bin/agentctl.exe"

type process_result = { code : int; stdout : string; stderr : string }

let spawn ?(stdin_text = "") args =
  let in_path = Filename.temp_file "agentctl-decide-stdin-" ".json" in
  let out_path = Filename.temp_file "agentctl-decide-stdout-" ".log" in
  let err_path = Filename.temp_file "agentctl-decide-stderr-" ".log" in
  write_file in_path stdin_text;
  let in_fd = Unix.openfile in_path [ Unix.O_RDONLY ] 0o600 in
  let out_fd =
    Unix.openfile out_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let err_fd =
    Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  let argv = Array.of_list (binary :: args) in
  let pid =
    try Unix.create_process binary argv in_fd out_fd err_fd
    with error ->
      Unix.close in_fd;
      Unix.close out_fd;
      Unix.close err_fd;
      raise error
  in
  Unix.close in_fd;
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
  Sys.remove in_path;
  Sys.remove out_path;
  Sys.remove err_path;
  { code; stdout; stderr }

let decide_args extra = [ "decide"; "--source"; ".."; "--json" ] @ extra

let test_cli_decisions () =
  Unix.putenv "NOMIA_TODAY" "2030-01-01";
  let deny_request =
    "{\"action\":\"fs.read\",\"tool\":\"read\",\"role\":\"implementer\",\
     \"paths\":[\"config/.env\"],\"network_hosts\":[]}"
  in
  let denied =
    spawn ~stdin_text:deny_request
      (decide_args [ "--client"; "opencode"; "--role"; "implementer" ])
  in
  Alcotest.(check int) "credential read exits 3" 3 denied.code;
  Alcotest.(check string) "deny has no stderr" "" denied.stderr;
  let deny_json = Yojson.Safe.from_string denied.stdout in
  Alcotest.(check string) "CLI deny decision" "deny"
    (member "decision" deny_json |> Option.value ~default:`Null |> jstring);
  Alcotest.(check bool) "CLI deny names the rule" true
    (contains_sub denied.stdout "security.credentials");
  let ask_request =
    "{\"action\":\"vcs.push\",\"tool\":\"git\",\"role\":\"implementer\",\
     \"paths\":[],\"network_hosts\":[]}"
  in
  let asked =
    spawn ~stdin_text:ask_request
      (decide_args [ "--client"; "opencode"; "--role"; "implementer" ])
  in
  Alcotest.(check int) "git push exits 4" 4 asked.code;
  Alcotest.(check bool) "CLI ask names the rule" true
    (contains_sub asked.stdout "operations.authority");
  let allow_request =
    "{\"action\":\"custom.unknown\",\"tool\":\"custom\",\"role\":\"implementer\",\
     \"paths\":[],\"network_hosts\":[]}"
  in
  let allowed =
    spawn ~stdin_text:allow_request
      (decide_args [ "--client"; "opencode"; "--role"; "implementer" ])
  in
  Alcotest.(check int) "unmatched action exits 0" 0 allowed.code;
  Alcotest.(check string) "CLI allow decision" "allow"
    (member "decision" (Yojson.Safe.from_string allowed.stdout)
    |> Option.value ~default:`Null |> jstring);
  let human =
    spawn ~stdin_text:deny_request
      [ "decide"; "--source"; ".."; "--client"; "opencode"; "--role";
        "implementer" ]
  in
  Alcotest.(check int) "human deny exits 3" 3 human.code;
  Alcotest.(check bool) "human line names the decision" true
    (contains_sub human.stdout "deny");
  let human_body = String.trim human.stdout in
  Alcotest.(check bool) "human line has no inner newline" false
    (String.contains human_body '\n');
  with_temp_dir (fun temporary ->
      let path = Filename.concat temporary "request.json" in
      write_file path ask_request;
      let from_file =
        spawn
          [ "decide"; "--source"; ".."; "--client"; "opencode"; "--role";
            "implementer"; "--request"; path ]
      in
      Alcotest.(check int) "request file exits 4" 4 from_file.code;
      Alcotest.(check bool) "request file names the rule" true
        (contains_sub from_file.stdout "operations.authority");
      let mismatch =
        spawn
          ~stdin_text:
            "{\"action\":\"fs.read\",\"role\":\"implementer\",\
             \"policy_digest\":\"sha256:0000000000000000000000000000000000000000000000000000000000000000\",\
             \"paths\":[\"src/x.ml\"]}"
          [ "decide"; "--source"; ".."; "--client"; "opencode" ]
      in
      Alcotest.(check int) "digest mismatch exits 1" 1 mismatch.code;
      Alcotest.(check bool) "digest mismatch is diagnosed" true
        (contains_sub mismatch.stderr "enforcement.policy-digest");
      let malformed =
        spawn ~stdin_text:"{" [ "decide"; "--source"; ".." ]
      in
      Alcotest.(check int) "malformed JSON exits 1" 1 malformed.code;
      Alcotest.(check bool) "malformed JSON is diagnosed" true
        (contains_sub malformed.stderr "enforcement.request.json"))

(* ------------------------------------------------------------------ *)
(* Suite                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia enforcement"
    [
      ( "decisions",
        [
          Alcotest.test_case "credential read denies" `Quick
            test_credential_deny;
          Alcotest.test_case "project context mismatch" `Quick
            test_project_context_mismatch;
          Alcotest.test_case "ordinary read allows" `Quick
            test_ordinary_read_allows;
          Alcotest.test_case "missing context asks" `Quick
            test_credential_missing_context_asks;
          Alcotest.test_case "authority asks" `Quick test_authority_asks;
          Alcotest.test_case "gates allow with obligations" `Quick
            test_gates_allow_with_obligations;
          Alcotest.test_case "no matching rule" `Quick test_no_match_allows;
        ] );
      ( "precedence",
        [
          Alcotest.test_case "deny wins over ask" `Quick test_deny_precedence;
          Alcotest.test_case "unsupported never allows" `Quick
            test_unsupported_never_allows;
          Alcotest.test_case "policy digest mismatch" `Quick
            test_policy_digest_mismatch;
        ] );
      ( "request",
        [ Alcotest.test_case "strict decoding" `Quick test_request_decoding ] );
      ( "output",
        [ Alcotest.test_case "deterministic and leak-free" `Quick
            test_deterministic_json ] );
      ("cli", [ Alcotest.test_case "exit codes and input" `Quick test_cli_decisions ]);
    ]
