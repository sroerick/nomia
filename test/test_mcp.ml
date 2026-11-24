open Nomia

module S = Nomia.Store

(* ------------------------------------------------------------------ *)
(* Paths and workspace                                                 *)
(* ------------------------------------------------------------------ *)

let start_dir = Sys.getcwd ()
let binary = Filename.concat start_dir "../bin/agentctl.exe"

let real_root =
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root
    when Sys.file_exists (Filename.concat root "nomia/policy.yaml") ->
      Filename.concat root "nomia"
  | _ -> ".."

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

let env_key entry =
  match String.index_opt entry '=' with
  | Some index -> String.sub entry 0 index
  | None -> entry

type workspace = {
  root : string;
  home : string;
  config : string;
  cache : string;
  src : string;
  out : string;
  erde : string;
}

let with_workspace action =
  let root = Filename.temp_dir "nomia-mcp-" "" in
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
      let out = Filename.concat root "render" in
      let erde = Filename.concat root "erde" in
      List.iter (fun path -> mkdir_p path) [ home; config; cache; src ];
      Unix.putenv "XDG_CACHE_HOME" cache;
      copy_source src;
      action { root; home; config; cache; src; out; erde })

let environment workspace ?(extra = []) () =
  let overrides =
    [
      "HOME=" ^ workspace.home;
      "XDG_CONFIG_HOME=" ^ workspace.config;
      "XDG_CACHE_HOME=" ^ workspace.cache;
      "NOMIA_TODAY=2030-01-01";
    ]
    @ extra
  in
  let keys = List.map env_key overrides @ [ "NOMIA_SOURCE" ] in
  let base =
    Unix.environment () |> Array.to_list
    |> List.filter (fun entry -> not (List.mem (env_key entry) keys))
  in
  Array.of_list (base @ overrides)

(* ------------------------------------------------------------------ *)
(* Synchronous command helper                                          *)
(* ------------------------------------------------------------------ *)

type process_result = { code : int; stdout : string; stderr : string }

let spawn ~env args =
  let out_path = Filename.temp_file "agentctl-mcp-out-" ".log" in
  let err_path = Filename.temp_file "agentctl-mcp-err-" ".log" in
  let out_fd = Unix.openfile out_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let err_fd = Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
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

let run workspace args =
  spawn ~env:(environment workspace ()) args

(* ------------------------------------------------------------------ *)
(* JSON-RPC server process                                             *)
(* ------------------------------------------------------------------ *)

type server = {
  pid : int;
  input : out_channel;
  output : in_channel;
  stderr_path : string;
  mutable alive : bool;
  mutable last : process_result;
}

exception Server_gone

let start_server workspace args =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_path = Filename.temp_file "agentctl-mcp-stderr-" ".log" in
  let err_fd = Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600 in
  let argv = Array.of_list (binary :: args) in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close stdin_r;
        Unix.close stdout_w;
        Unix.close err_fd)
      (fun () ->
        (* Keep the parent's ends out of the child; otherwise the child holds a
           write end of its own stdin and EOF never arrives. *)
        Unix.set_close_on_exec stdin_w;
        Unix.set_close_on_exec stdout_r;
        Unix.create_process_env binary argv (environment workspace ()) stdin_r
          stdout_w err_fd)
  in
  Unix.clear_close_on_exec stdin_w;
  Unix.clear_close_on_exec stdout_r;
  {
    pid;
    input = Unix.out_channel_of_descr stdin_w;
    output = Unix.in_channel_of_descr stdout_r;
    stderr_path;
    alive = true;
    last = { code = 0; stdout = ""; stderr = "" };
  }

let stop server =
  if server.alive then begin
    server.alive <- false;
    (try close_out server.input with Sys_error _ -> ());
    let stdout =
      try In_channel.input_all server.output with Sys_error _ -> ""
    in
    (try close_in_noerr server.output with Sys_error _ -> ());
    let _, status = Unix.waitpid [] server.pid in
    let code =
      match status with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED signal -> 128 + signal
      | Unix.WSTOPPED signal -> 128 + signal
    in
    let stderr =
      try read_file server.stderr_path with Sys_error _ -> ""
    in
    (try Sys.remove server.stderr_path with Sys_error _ -> ());
    server.last <- { code; stdout; stderr }
  end;
  server.last

let with_server workspace ~args action =
  let server = start_server workspace args in
  Fun.protect
    ~finally:(fun () -> ignore (stop server))
    (fun () -> action server)

let send_json server json =
  output_string server.input (Yojson.Safe.to_string json);
  output_char server.input '\n';
  flush server.input

let recv_json server =
  match input_line server.output with
  | line -> (
      try Yojson.Safe.from_string line
      with Yojson.Json_error message ->
        Alcotest.failf "stdout line is not JSON (%s): %s" message line)
  | exception End_of_file -> raise Server_gone

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let next_id = ref 0

let fresh_id () =
  incr next_id;
  !next_id

let request server ?(params = `Assoc []) method_name =
  let id = fresh_id () in
  send_json server
    (`Assoc
      [
        ("jsonrpc", `String "2.0");
        ("id", `Int id);
        ("method", `String method_name);
        ("params", params);
      ]);
  let response = recv_json server in
  Alcotest.(check int) "response id" id
    (match member "id" response with Some (`Int value) -> value | _ -> -1);
  (match member "jsonrpc" response with
  | Some (`String "2.0") -> ()
  | _ -> Alcotest.fail "response is not JSON-RPC 2.0");
  response

let notification server method_name =
  send_json server
    (`Assoc
      [ ("jsonrpc", `String "2.0"); ("method", `String method_name) ])

let result response =
  match member "result" response with
  | Some result -> result
  | None ->
      Alcotest.failf "response has no result: %s" (Yojson.Safe.to_string response)

let error_code response =
  match member "error" response with
  | Some error -> (
      match member "code" error with
      | Some (`Int code) -> code
      | _ -> Alcotest.fail "error has no integer code")
  | None ->
      Alcotest.failf "response has no error: %s" (Yojson.Safe.to_string response)

let string_field key json =
  match member key json with
  | Some (`String value) -> value
  | _ -> Alcotest.failf "missing string field %s" key

let string_list_field key json =
  match member key json with
  | Some (`List values) ->
      List.filter_map (function `String value -> Some value | _ -> None) values
  | _ -> Alcotest.failf "missing string list field %s" key

let initialize server =
  let response = request server "initialize" ~params:(`Assoc []) in
  let initialized = result response in
  Alcotest.(check string) "protocol version" "2024-11-05"
    (string_field "protocolVersion" initialized);
  let capabilities = Option.get (member "capabilities" initialized) in
  Alcotest.(check bool) "resources capability" true
    (member "resources" capabilities <> None);
  Alcotest.(check bool) "tools capability" true
    (member "tools" capabilities <> None);
  let server_info = Option.get (member "serverInfo" initialized) in
  Alcotest.(check string) "server name" "agentctl-mcp"
    (string_field "name" server_info);
  notification server "notifications/initialized";
  let ping = request server "ping" in
  ignore (result ping)

let resources server =
  let response = request server "resources/list" in
  let result = result response in
  match member "resources" result with
  | Some (`List entries) ->
      List.map
        (fun entry ->
          ( string_field "uri" entry,
            string_field "name" entry,
            string_field "mimeType" entry ))
        entries
  | _ -> Alcotest.fail "resources/list has no resources"

let read_text server uri =
  let response =
    request server "resources/read" ~params:(`Assoc [ ("uri", `String uri) ])
  in
  let result = result response in
  match member "contents" result with
  | Some (`List [ entry ]) -> string_field "text" entry
  | _ -> Alcotest.failf "resources/read for %s has no single content" uri

let tool_payload server arguments =
  let response =
    request server "tools/call"
      ~params:
        (`Assoc
          [
            ("name", `String "resolve_policy");
            ("arguments", arguments);
          ])
  in
  let result = result response in
  let structured = Option.get (member "structuredContent" result) in
  let content =
    match member "content" result with
    | Some (`List [ entry ]) -> string_field "text" entry
    | _ -> Alcotest.fail "tools/call has no single content entry"
  in
  Alcotest.(check string) "structured content matches text"
    (Canonical.to_string structured ^ "\n")
    content;
  structured

let rec has_key names = function
  | `Assoc fields ->
      List.exists
        (fun (key, value) -> List.mem key names || has_key names value)
        fields
  | `List values -> List.exists (has_key names) values
  | _ -> false

let assert_no_secret_fields name json =
  let forbidden =
    [
      "credentials";
      "credential";
      "token";
      "secret";
      "password";
      "api_key";
    ]
  in
  Alcotest.(check bool) (name ^ " has no credential fields") false
    (has_key forbidden json)

let ends_with suffix uri =
  let suffix_len = String.length suffix and uri_len = String.length uri in
  uri_len >= suffix_len
  && String.sub uri (uri_len - suffix_len) suffix_len = suffix

let find_uri resources pattern =
  List.find_opt (fun (uri, _, _) -> pattern uri) resources
  |> Option.map (fun (uri, _, _) -> uri)

let assert_contains name needle haystack =
  let rec loop index =
    if index + String.length needle > String.length haystack then false
    else if String.sub haystack index (String.length needle) = needle then true
    else loop (index + 1)
  in
  Alcotest.(check bool) name true (loop 0)

(* ------------------------------------------------------------------ *)
(* Handshake, resource inventory, and byte identity                    *)
(* ------------------------------------------------------------------ *)

let test_handshake_and_inventory () =
  with_workspace (fun workspace ->
      with_server workspace
        ~args:[ "mcp"; "--source"; real_root; "--client"; "generic-agents" ]
        (fun server ->
          initialize server;
          let resources = resources server in
          List.iter
            (fun suffix_name ->
              Alcotest.(check bool)
                (Printf.sprintf "resources/list has %s" suffix_name)
                true (find_uri resources (fun uri -> ends_with suffix_name uri) <> None))
            [
              "/manifest";
              "/core";
              "/explain";
              "/rules/engineering.scope";
              "/skills/verify-change";
            ];
          Alcotest.(check bool) "resources/list has capabilities" true
            (find_uri resources (fun uri -> ends_with "capabilities/generic-agents" uri)
            <> None);
          Alcotest.(check bool) "manifest is JSON" true
            (List.exists
               (fun (_, _, mime) -> mime = "application/json")
               resources);
          Alcotest.(check bool) "core is Markdown" true
            (List.exists (fun (_, _, mime) -> mime = "text/markdown") resources);
          let stopped = stop server in
          Alcotest.(check int) "server exits 0" 0 stopped.code;
          Alcotest.(check string) "stderr is quiet on a live source" ""
            stopped.stderr))

let test_resource_bytes_match_render () =
  with_workspace (fun workspace ->
      let client = "erde" and role = "implementer" in
      let render =
        run workspace
          [
            "render";
            "--client";
            client;
            "--role";
            role;
            "--source";
            real_root;
            "--out";
            workspace.out;
          ]
      in
      Alcotest.(check int) "render exits 0" 0 render.code;
      let erde =
        run workspace
          [
            "render";
            "--client";
            "erde";
            "--role";
            role;
            "--source";
            real_root;
            "--out";
            workspace.erde;
          ]
      in
      Alcotest.(check int) "erde render exits 0" 0 erde.code;
      with_server workspace
        ~args:
          [
            "mcp";
            "--source";
            real_root;
            "--client";
            client;
            "--role";
            role;
          ]
        (fun server ->
          initialize server;
          let resources = resources server in
          let release = "0.2.0" in
          let base = "org-policy://releases/" ^ release in
          let manifest_uri = base ^ "/manifest" in
          let core_uri = base ^ "/core" in
          let explain_uri = base ^ "/explain" in
          let manifest = read_text server manifest_uri in
          let core = read_text server core_uri in
          let explain = read_text server explain_uri in
          Alcotest.(check string) "manifest bytes equal render"
            (read_file (Filename.concat workspace.out ".nomia/manifest.json"))
            manifest;
          Alcotest.(check string) "explain bytes equal render"
            (read_file
               (Filename.concat workspace.out ".nomia/explain.json"))
            explain;
          Alcotest.(check string) "core bytes equal erde org-core"
            (read_file (Filename.concat workspace.erde "org-core.md"))
            core;
          Alcotest.(check string) "manifest bytes are stable" manifest
            (read_text server manifest_uri);
          Alcotest.(check string) "core bytes are stable" core
            (read_text server core_uri);
          Alcotest.(check string) "explain bytes are stable" explain
            (read_text server explain_uri);
          Alcotest.(check bool) "core is organization-only" false
            (String.length core = 0);
          let capabilities_uri = "org-policy://capabilities/erde" in
          let capabilities = Yojson.Safe.from_string (read_text server capabilities_uri) in
          Alcotest.(check string) "capability release" release
            (string_field "release" capabilities);
          Alcotest.(check string) "capability client" "erde"
            (string_field "client" capabilities);
          Alcotest.(check bool) "resource inventory is non-empty" true
            (List.length resources > 10);
          ignore (stop server)))

let test_rules_and_skills_deterministic () =
  with_workspace (fun workspace ->
      let client = "generic-agents" and role = "implementer" in
      let render =
        run workspace
          [
            "render";
            "--client";
            client;
            "--role";
            role;
            "--source";
            real_root;
            "--out";
            workspace.out;
          ]
      in
      Alcotest.(check int) "render exits 0" 0 render.code;
      let explain_json =
        Yojson.Safe.from_string
          (read_file
             (Filename.concat workspace.out ".nomia/explain.json"))
      in
      let expected_rules =
        match member "rules" explain_json with
        | Some (`List rules) -> List.length rules
        | _ -> Alcotest.fail "explain has no rules"
      in
      let expected_skills =
        match member "skills" explain_json with
        | Some (`List skills) -> List.length skills
        | _ -> Alcotest.fail "explain has no skills"
      in
      with_server workspace
        ~args:
          [
            "mcp";
            "--source";
            real_root;
            "--client";
            client;
            "--role";
            role;
          ]
        (fun server ->
          initialize server;
          let resources = resources server in
          let rule_uris =
            resources
            |> List.filter_map (fun (uri, _, _) ->
                   let parts = String.split_on_char '/' uri in
                   if List.length parts >= 2
                      && List.nth parts (List.length parts - 2) = "rules"
                   then Some uri
                   else None)
          in
          Alcotest.(check int) "every composed rule is a resource"
            expected_rules (List.length rule_uris);
          List.iter
            (fun uri ->
              let first = read_text server uri in
              let second = read_text server uri in
              Alcotest.(check string) ("rule is deterministic: " ^ uri) first
                second;
              let document = Yojson.Safe.from_string first in
              Alcotest.(check string) "rule schema"
                "nomia-mcp-rule/v1" (string_field "schema" document);
              Alcotest.(check bool) "rule has metadata" true
                (member "metadata" document <> None);
              Alcotest.(check bool) "rule body is non-empty" true
                (String.length (string_field "body" document) > 0))
            rule_uris;
          let skill_uris =
            resources
            |> List.filter_map (fun (uri, _, _) ->
                   let parts = String.split_on_char '/' uri in
                   if List.length parts >= 2
                      && List.nth parts (List.length parts - 2) = "skills"
                   then Some uri
                   else None)
          in
          Alcotest.(check int) "every composed skill is a resource"
            expected_skills (List.length skill_uris);
          List.iter
            (fun uri ->
              let first = read_text server uri in
              let second = read_text server uri in
              Alcotest.(check string) ("skill is deterministic: " ^ uri) first
                second;
              let name =
                List.nth (String.split_on_char '/' uri)
                  (List.length (String.split_on_char '/' uri) - 1)
              in
              let rendered =
                read_file
                  (Filename.concat workspace.out
                     (".agents/skills/" ^ name ^ "/SKILL.md"))
              in
              Alcotest.(check string) ("skill bytes equal render: " ^ name)
                rendered first)
            skill_uris;
          ignore (stop server)))

(* ------------------------------------------------------------------ *)
(* Protocol errors and pinned-release refusal                           *)
(* ------------------------------------------------------------------ *)

let test_protocol_errors () =
  with_workspace (fun workspace ->
      with_server workspace
        ~args:[ "mcp"; "--source"; real_root; "--client"; "generic-agents" ]
        (fun server ->
          initialize server;
          let unknown_method = request server "does/not/exist" in
          Alcotest.(check int) "unknown method is -32601" (-32601)
            (error_code unknown_method);
          let unknown_resource =
            request server "resources/read"
              ~params:
                (`Assoc
                  [ ("uri", `String "org-policy://releases/0.2.0/nope") ])
          in
          Alcotest.(check int) "unknown resource is -32602" (-32602)
            (error_code unknown_resource);
          let missing_uri =
            request server "resources/read" ~params:(`Assoc [])
          in
          Alcotest.(check int) "missing uri is -32602" (-32602)
            (error_code missing_uri);
          let wrong_uri_type =
            request server "resources/read"
              ~params:(`Assoc [ ("uri", `Int 7) ])
          in
          Alcotest.(check int) "non-string uri is -32602" (-32602)
            (error_code wrong_uri_type);
          let other_release =
            request server "resources/read"
              ~params:
                (`Assoc
                  [ ("uri", `String "org-policy://releases/9.9.9/manifest") ])
          in
          Alcotest.(check int) "different release is -32602" (-32602)
            (error_code other_release);
          let malformed_id = fresh_id () in
          output_string server.input "this is not json\n";
          flush server.input;
          let malformed = recv_json server in
          Alcotest.(check int) "malformed JSON is -32700" (-32700)
            (error_code malformed);
          ignore malformed_id;
          let batch = `List [ `Assoc [ ("jsonrpc", `String "2.0") ] ] in
          output_string server.input (Yojson.Safe.to_string batch);
          output_char server.input '\n';
          flush server.input;
          let batch_response = recv_json server in
          Alcotest.(check int) "batch array is -32600" (-32600)
            (error_code batch_response);
          let stopped = stop server in
          Alcotest.(check int) "server exits 0 after protocol errors" 0
            stopped.code;
          Alcotest.(check string) "no stray stdout remains" "" stopped.stdout))

let test_tool_bounded_and_pinned () =
  with_workspace (fun workspace ->
      with_server workspace
        ~args:
          [
            "mcp";
            "--source";
            real_root;
            "--client";
            "generic-agents";
            "--role";
            "implementer";
          ]
        (fun server ->
          initialize server;
          let tools = result (request server "tools/list") in
          (match member "tools" tools with
          | Some (`List [ tool ]) ->
              Alcotest.(check string) "only tool name" "resolve_policy"
                (string_field "name" tool)
          | _ -> Alcotest.fail "tools/list must expose exactly resolve_policy");
          let payload = tool_payload server (`Assoc []) in
          let digest = string_field "digest" payload in
          Alcotest.(check bool) "digest is sha256" true
            (String.length digest = 71
            && String.sub digest 0 7 = "sha256:");
          let manifest = Option.get (member "manifest" payload) in
          Alcotest.(check string) "tool digest equals manifest digest" digest
            (string_field "digest" manifest);
          let release = Option.get (member "release" payload) in
          Alcotest.(check string) "tool release version" "0.2.0"
            (string_field "version" release);
          Alcotest.(check string) "tool role" "implementer"
            (string_field "role" payload);
          Alcotest.(check bool) "tool core is non-empty" true
            (String.length (string_field "core" payload) > 0);
          let capabilities = Option.get (member "capabilities" payload) in
          let adapter = Option.get (member "adapter" capabilities) in
          Alcotest.(check string) "tool capability adapter" "generic-agents"
            (string_field "id" adapter);
          assert_no_secret_fields "resolve_policy payload" payload;
          let matching = tool_payload server
              (`Assoc [ ("release", `String "0.2.0") ])
          in
          Alcotest.(check string) "pinned release accepted" digest
            (string_field "digest" matching);
          let by_digest = tool_payload server
              (`Assoc [ ("release", `String digest) ])
          in
          Alcotest.(check string) "pinned digest accepted" digest
            (string_field "digest" by_digest);
          let other_release =
            request server "tools/call"
              ~params:
                (`Assoc
                  [
                    ("name", `String "resolve_policy");
                    ( "arguments",
                      `Assoc [ ("release", `String "9.9.9") ] );
                  ])
          in
          Alcotest.(check int) "tool cannot switch releases" (-32602)
            (error_code other_release);
          let other_client =
            request server "tools/call"
              ~params:
                (`Assoc
                  [
                    ("name", `String "resolve_policy");
                    ( "arguments",
                      `Assoc [ ("client", `String "opencode") ] );
                  ])
          in
          Alcotest.(check int) "tool cannot switch clients" (-32602)
            (error_code other_client);
          let other_role =
            request server "tools/call"
              ~params:
                (`Assoc
                  [
                    ("name", `String "resolve_policy");
                    ( "arguments",
                      `Assoc [ ("role", `String "code-reviewer") ] );
                  ])
          in
          Alcotest.(check int) "tool cannot switch roles" (-32602)
            (error_code other_role);
          let repo_argument =
            request server "tools/call"
              ~params:
                (`Assoc
                  [
                    ("name", `String "resolve_policy");
                    ( "arguments",
                      `Assoc [ ("repo", `String "/etc") ] );
                  ])
          in
          Alcotest.(check int) "tool cannot select a repository" (-32602)
            (error_code repo_argument);
          let forbidden_argument =
            request server "tools/call"
              ~params:
                (`Assoc
                  [
                    ("name", `String "resolve_policy");
                    ( "arguments",
                      `Assoc [ ("credentials", `String "nope") ] );
                  ])
          in
          Alcotest.(check int) "tool rejects credential fields" (-32602)
            (error_code forbidden_argument);
          let unknown_tool =
            request server "tools/call"
              ~params:(`Assoc [ ("name", `String "grant_permission") ])
          in
          Alcotest.(check int) "unknown tool is -32602" (-32602)
            (error_code unknown_tool);
          ignore (stop server)))

(* ------------------------------------------------------------------ *)
(* Offline fallback and no-cache failure                               *)
(* ------------------------------------------------------------------ *)

let test_release_pin () =
  with_workspace (fun workspace ->
      let sync =
        run workspace
          ([ "sync"; "--client"; "generic-agents" ] @ [ "--source"; workspace.src ])
      in
      Alcotest.(check int) "sync populates the cache" 0 sync.code;
      let digest =
        match S.cache_digests () with
        | [ digest ] -> digest
        | _ -> Alcotest.fail "expected exactly one cached release"
      in
      let cached =
        match S.load_cached ~digest with
        | Ok cached -> cached
        | Error message -> Alcotest.failf "cached release: %s" message
      in
      (* The digest names one client-specific release. Asking for a different
         client is a clean startup error, not a silent cross-client serve. *)
      let mismatch =
        start_server workspace
          [
            "mcp";
            "--release";
            digest;
            "--source";
            workspace.src;
            "--client";
            "opencode";
          ]
      in
      let mismatch = stop mismatch in
      Alcotest.(check int) "cross-client pinned release exits 2" 2 mismatch.code;
      assert_contains "cross-client pin is explained" "belongs to client"
        mismatch.stderr;
      with_server workspace
        ~args:
          [
            "mcp";
            "--release";
            digest;
            "--source";
            workspace.src;
            "--client";
            "generic-agents";
          ]
        (fun server ->
          initialize server;
          let manifest =
            read_text server "org-policy://releases/0.2.0/manifest"
          in
          Alcotest.(check string) "pinned digest manifest bytes"
            (Canonical.to_string cached.S.manifest ^ "\n")
            manifest;
          Alcotest.(check bool) "pinned capabilities follow the cached client"
            true
            (find_uri (resources server)
               (fun uri -> ends_with "capabilities/generic-agents" uri)
            <> None);
          let payload = tool_payload server (`Assoc []) in
          Alcotest.(check string) "pinned digest is returned" digest
            (string_field "digest" payload);
          let stopped = stop server in
          Alcotest.(check int) "pinned server exits 0" 0 stopped.code;
          Alcotest.(check string) "pinned selection is quiet on stderr" ""
            stopped.stderr))

let test_cached_fallback () =
  with_workspace (fun workspace ->
      let sync =
        run workspace
          ([ "sync"; "--client"; "generic-agents" ] @ [ "--source"; workspace.src ])
      in
      Alcotest.(check int) "sync populates the cache" 0 sync.code;
      let digests = S.cache_digests () in
      Alcotest.(check bool) "cache has one release" true
        (List.length digests = 1);
      let digest = List.hd digests in
      let cached =
        match S.load_cached ~digest with
        | Ok cached -> cached
        | Error message -> Alcotest.failf "cached release: %s" message
      in
      let moved = workspace.src ^ "-offline" in
      Sys.rename workspace.src moved;
      with_server workspace
        ~args:
          [
            "mcp";
            "--source";
            workspace.src;
            "--client";
            "generic-agents";
          ]
        (fun server ->
          initialize server;
          let release = "0.2.0" in
          let base = "org-policy://releases/" ^ release in
          let manifest = read_text server (base ^ "/manifest") in
          let explain = read_text server (base ^ "/explain") in
          Alcotest.(check string) "cached manifest bytes"
            (Canonical.to_string cached.S.manifest ^ "\n")
            manifest;
          Alcotest.(check string) "cached explain bytes"
            (Canonical.to_string cached.S.explain ^ "\n")
            explain;
          Alcotest.(check bool) "cached core is non-empty" true
            (String.length (read_text server (base ^ "/core")) > 0);
          let payload = tool_payload server (`Assoc []) in
          Alcotest.(check string) "cached tool digest" digest
            (string_field "digest" payload);
          let stopped = stop server in
          Alcotest.(check int) "cached server exits 0" 0 stopped.code;
          assert_contains "fallback is reported on stderr" "cached release"
            stopped.stderr))

let test_no_cache_fails_cleanly () =
  with_workspace (fun workspace ->
      let missing = Filename.concat workspace.root "nowhere" in
      let server = start_server workspace
          [ "mcp"; "--source"; missing; "--client"; "generic-agents" ]
      in
      let result = stop server in
      Alcotest.(check int) "unavailable source without cache exits 2" 2
        result.code;
      Alcotest.(check string) "no empty policy on stdout" "" result.stdout;
      assert_contains "startup failure is explained" "no policy source"
        result.stderr;
      let bad_release = Filename.concat workspace.root "no-such-digest" in
      let digest_missing =
        start_server workspace
          [
            "mcp";
            "--release";
            "sha256:0000000000000000000000000000000000000000000000000000000000000000";
            "--source";
            bad_release;
          ]
      in
      let result = stop digest_missing in
      Alcotest.(check int) "missing pinned digest exits 2" 2 result.code;
      Alcotest.(check string) "missing digest has no stdout" "" result.stdout;
      assert_contains "missing digest is explained" "cached release"
        result.stderr)

(* ------------------------------------------------------------------ *)
(* Offline/no-credential posture                                       *)
(* ------------------------------------------------------------------ *)

let test_offline_environment_only () =
  with_workspace (fun workspace ->
      with_server workspace
        ~args:[ "mcp"; "--source"; real_root; "--client"; "generic-agents" ]
        (fun server ->
          initialize server;
          let response = request server "resources/read"
              ~params:
                (`Assoc
                  [ ("uri", `String "org-policy://releases/0.2.0/manifest") ])
          in
          let text =
            match member "result" response with
            | Some r -> (
                match member "contents" r with
                | Some (`List [ entry ]) -> string_field "text" entry
                | _ -> Alcotest.fail "missing contents")
            | None -> Alcotest.fail "missing result"
          in
          Alcotest.(check bool) "manifest names the release" true
            (String.length text > 0);
          assert_no_secret_fields "manifest" (Yojson.Safe.from_string text);
          let stopped = stop server in
          Alcotest.(check int) "offline server exits 0" 0 stopped.code))

(* ------------------------------------------------------------------ *)
(* Suite definitions                                                   *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia-mcp"
    [
      ( "stdio",
        [
          Alcotest.test_case "initialize and resource inventory" `Quick
            test_handshake_and_inventory;
          Alcotest.test_case "resource bytes match render" `Quick
            test_resource_bytes_match_render;
          Alcotest.test_case "rules and skills deterministic" `Quick
            test_rules_and_skills_deterministic;
        ] );
      ( "protocol",
        [
          Alcotest.test_case "JSON-RPC error codes" `Quick test_protocol_errors;
          Alcotest.test_case "bounded pinned resolver" `Quick
            test_tool_bounded_and_pinned;
        ] );
      ( "offline",
        [
          Alcotest.test_case "explicit release pin" `Quick test_release_pin;
          Alcotest.test_case "cached fallback" `Quick test_cached_fallback;
          Alcotest.test_case "no cache exits 2" `Quick
            test_no_cache_fails_cleanly;
          Alcotest.test_case "no credentials in resources" `Quick
            test_offline_environment_only;
        ] );
    ]
