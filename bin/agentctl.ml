(* agentctl -- validate, render, and explain the reviewed agent policy.

   Every command is offline: no provider, credential, production service, or
   native client configuration is touched. Only [render --out DIR] writes, and
   only inside DIR. *)

open Cmdliner
open Nomia

(* ------------------------------------------------------------------ *)
(* Small output helpers                                                *)
(* ------------------------------------------------------------------ *)

let compare_diagnostic left right =
  match String.compare left.Model.path right.Model.path with
  | 0 -> (
      match (left.Model.line, right.Model.line) with
      | None, None -> String.compare left.Model.code right.Model.code
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some left_line, Some right_line ->
          let by_line = Int.compare left_line right_line in
          if by_line <> 0 then by_line
          else String.compare left.Model.code right.Model.code)
  | order -> order

let sorted_diagnostics diagnostics =
  List.sort
    (fun left right ->
      let by_position = compare_diagnostic left right in
      if by_position <> 0 then by_position
      else String.compare left.Model.message right.Model.message)
    diagnostics

let diagnostic_line (diagnostic : Model.diagnostic) =
  match diagnostic.line with
  | Some line ->
      Printf.sprintf "%s:%d: %s: %s" diagnostic.path line diagnostic.code
        diagnostic.message
  | None ->
      Printf.sprintf "%s: %s: %s" diagnostic.path diagnostic.code
        diagnostic.message

let print_diagnostics channel diagnostics =
  sorted_diagnostics diagnostics
  |> List.iter (fun diagnostic ->
         output_string channel (diagnostic_line diagnostic);
         output_char channel '\n')

let diagnostic_json (diagnostic : Model.diagnostic) =
  `Assoc
    [
      ("code", `String diagnostic.code);
      ("path", `String diagnostic.path);
      ( "line",
        match diagnostic.line with None -> `Null | Some line -> `Int line );
      ("message", `String diagnostic.message);
    ]

let check_json diagnostics =
  `Assoc
    [
      ("ok", `Bool (diagnostics = []));
      ("diagnostics", `List (List.map diagnostic_json
                               (sorted_diagnostics diagnostics)));
    ]

let report_error message = prerr_endline ("agentctl: " ^ message)

let guard action =
  try action () with
  | Sys_error message -> report_error message; 1
  | Invalid_argument message -> report_error message; 1
  | Unix.Unix_error (error, function_name, argument) ->
      report_error
        (Printf.sprintf "%s: %s %s" (Unix.error_message error) function_name
           argument);
      1

(* ------------------------------------------------------------------ *)
(* Source discovery                                                    *)
(* ------------------------------------------------------------------ *)

let nonempty value =
  match value with
  | Some value when String.trim value <> "" -> Some value
  | _ -> None

let rec walk_for_policy dir =
  let candidate = Filename.concat dir "nomia/policy.yaml" in
  if Sys.file_exists candidate then Some (Filename.concat dir "nomia")
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else walk_for_policy parent

let discover_source ?source () =
  match nonempty source with
  | Some root -> Ok root
  | None -> (
      match nonempty (Sys.getenv_opt "NOMIA_SOURCE") with
      | Some root -> Ok root
      | None -> (
          match walk_for_policy (Sys.getcwd ()) with
          | Some root -> Ok root
          | None ->
              let cwd = Sys.getcwd () in
              if Sys.file_exists (Filename.concat cwd "policy.yaml") then
                Ok cwd
              else
                Error
                  "no policy source found; pass a source directory or set \
                   NOMIA_SOURCE"))

let discovery_diagnostic message : Model.diagnostic =
  {
    code = "source.discovery";
    path = ".";
    line = None;
    message;
  }

(* ------------------------------------------------------------------ *)
(* check                                                               *)
(* ------------------------------------------------------------------ *)

let check_run source json =
  guard (fun () ->
      match discover_source ?source () with
      | Error message ->
          let diagnostics = [ discovery_diagnostic message ] in
          if json then
            print_endline (Canonical.to_string (check_json diagnostics))
          else print_diagnostics stdout diagnostics;
          1
      | Ok root -> (
          match Source.load_and_validate ~root ?today:None with
          | Ok _ ->
              if json then
                print_endline (Canonical.to_string (check_json []));
              0
          | Error diagnostics ->
              if json then
                print_endline (Canonical.to_string (check_json diagnostics))
              else print_diagnostics stdout diagnostics;
              1))

(* ------------------------------------------------------------------ *)
(* render                                                              *)
(* ------------------------------------------------------------------ *)

let release_for_render (policy_set : Compose.t) artifacts =
  let role =
    Option.map (fun (role : Model.role) -> role.name)
      policy_set.Compose.role
  in
  let fragments =
    List.map
      (fun (fragment : Compose.fragment) ->
        (fragment.path, fragment.digest))
      policy_set.Compose.fragments
  in
  let exceptions =
    List.map
      (fun (exception_ : Model.exception_) -> exception_.id)
      policy_set.Compose.exceptions
  in
  Release.build ~client:policy_set.Compose.adapter.Model.id ?role
    ?project:policy_set.Compose.project
    ~inherit_root:policy_set.Compose.inherit_root ~fragments ~exceptions
    ~artifacts policy_set.Compose.source

let render_run client repo project inherit_root role source out =
  guard (fun () ->
      match discover_source ?source () with
      | Error message ->
          report_error message;
          1
      | Ok root -> (
          match
            Compose.resolve_root ?repo ?project ~inherit_root ?role ~client
              ?today:None ~root ()
          with
          | Error diagnostics ->
              print_diagnostics stderr diagnostics;
              1
          | Ok policy_set -> (
              match Render.check_out ~out with
              | Error message ->
                  report_error message;
                  1
              | Ok () -> (
                  match Render.render policy_set ~client with
                  | Error diagnostics ->
                      print_diagnostics stderr diagnostics;
                      1
                  | Ok artifacts ->
                      let artifacts_with_bytes =
                        List.map
                          (fun (artifact : Render.artifact) ->
                            (artifact.path, artifact.contents))
                          artifacts
                      in
                      let manifest =
                        release_for_render policy_set artifacts_with_bytes
                      in
                      let manifest_artifact : Render.artifact =
                        {
                          path = ".nomia/manifest.json";
                          contents = Release.to_string manifest ^ "\n";
                          mode = 0o644;
                        }
                      in
                      match
                        Render.write ~out (artifacts @ [ manifest_artifact ])
                      with
                      | Ok () -> 0
                      | Error message ->
                          report_error
                            ("cannot write rendered policy: " ^ message);
                          1))))

(* ------------------------------------------------------------------ *)
(* explain                                                             *)
(* ------------------------------------------------------------------ *)

let explain_run repo project inherit_root role client source json =
  guard (fun () ->
      match discover_source ?source () with
      | Error message ->
          report_error message;
          1
      | Ok root -> (
          match
            Compose.resolve_root ?repo ?project ~inherit_root ?role ?client
              ?today:None ~root ()
          with
          | Error diagnostics ->
              print_diagnostics stderr diagnostics;
              1
          | Ok policy_set ->
              if json then
                print_string
                  (Canonical.to_string (Compose.explain_json policy_set) ^ "\n")
              else print_string (Compose.explain_text policy_set);
              0))

(* ------------------------------------------------------------------ *)
(* sync                                                                *)
(* ------------------------------------------------------------------ *)

let source_available root =
  Sys.file_exists (Filename.concat root "policy.yaml")

let print_sync_error = function
  | Sync.Message message ->
      report_error message;
      1
  | Sync.Diagnostics diagnostics ->
      print_diagnostics stderr diagnostics;
      1

let sync_run repo project inherit_root client role source update_pin =
  guard (fun () ->
      match discover_source ?source () with
      | Error message ->
          report_error message;
          1
      | Ok root when not (source_available root) ->
          report_error
            "no policy source was found at the selected source directory; \
             pass --source DIR";
          1
      | Ok root -> (
          match
            Sync.sync ~repo ~project ~inherit_root ~client ~role
              ~source_root:root ~update_pin
          with
          | Error error -> print_sync_error error
          | Ok result ->
              let target =
                match (repo, project) with
                | Some _, Some project -> "repo project " ^ project
                | Some _, None -> "repo"
                | None, _ ->
                    "client "
                    ^ Option.value client ~default:(result.Sync.state.client)
              in
              Printf.printf "nomia sync\ntarget: %s\nrelease: %s\ndigest: %s\nentrypoints: %s\ncurrent: %s\nlock: %s\n"
                target result.Sync.state.release result.Sync.state.digest
                (String.concat ", " result.Sync.state.entrypoints)
                (if result.Sync.changed then "updated" else "unchanged")
                (if result.Sync.lock_updated then "updated" else "unchanged");
              List.iter
                (fun warning -> prerr_endline ("agentctl: warning: " ^ warning))
                result.Sync.warnings;
              0))

(* ------------------------------------------------------------------ *)
(* doctor                                                              *)
(* ------------------------------------------------------------------ *)

let doctor_run repo project inherit_root role client source json =
  guard (fun () ->
      let source_root =
        match discover_source ?source () with
        | Ok root when source_available root -> Some root
        | _ -> None
      in
      match
        Sync.doctor ~repo ~project ~inherit_root ~role ~client ~source_root
      with
      | Error (Sync.Message message) ->
          report_error message;
          1
      | Error (Sync.Diagnostics diagnostics) ->
          print_diagnostics stderr diagnostics;
          1
      | Ok report ->
          if json then
            print_endline
              (Canonical.to_string (Sync.doctor_to_json report))
          else print_string (Sync.doctor_text report);
          Sync.doctor_exit report)

(* ------------------------------------------------------------------ *)
(* decide                                                              *)
(* ------------------------------------------------------------------ *)

let read_request_file path =
  In_channel.with_open_bin path In_channel.input_all

let decide_run repo project role client source json request_file =
  guard (fun () ->
      match discover_source ?source () with
      | Error message ->
          report_error message;
          1
      | Ok root -> (
          let request_text =
            match request_file with
            | Some path -> (
                try Ok (read_request_file path)
                with Sys_error message -> Error message)
            | None -> Ok (In_channel.input_all In_channel.stdin)
          in
          match request_text with
          | Error message ->
              report_error message;
              1
          | Ok text -> (
              match Enforcement.request_of_string text with
              | Error diagnostics ->
                  print_diagnostics stderr diagnostics;
                  1
              | Ok request ->
                  let effective_role =
                    match role with
                    | Some _ -> role
                    | None -> request.Enforcement.role
                  in
                  let effective_project =
                    match project with
                    | Some _ -> project
                    | None -> request.Enforcement.project
                  in
                  match
                    Compose.resolve_root ?repo ?project:effective_project
                      ?role:effective_role ?client ?today:None ~root ()
                  with
                  | Error diagnostics ->
                      print_diagnostics stderr diagnostics;
                      1
                  | Ok policy_set -> (
                      match Enforcement.decide policy_set request with
                      | Error diagnostics ->
                          print_diagnostics stderr diagnostics;
                          1
                      | Ok result ->
                          if json then
                            print_endline (Enforcement.to_string result)
                          else print_endline (Enforcement.human_line result);
                          match result.Enforcement.decision with
                          | Enforcement.Allow -> 0
                          | Enforcement.Deny -> 3
                          | Enforcement.Ask -> 4))))

(* ------------------------------------------------------------------ *)
(* mcp                                                                 *)
(* ------------------------------------------------------------------ *)

let mcp_run source repo project role client release =
  guard (fun () ->
      Mcp.run ?source ?repo ?project ?role ?client ?release ())

(* ------------------------------------------------------------------ *)
(* version                                                             *)
(* ------------------------------------------------------------------ *)

let version_run () =
  guard (fun () ->
      let message =
        match discover_source () with
        | Error _ -> "agentctl dev"
        | Ok root -> (
            match Source.load ~root with
            | Ok source ->
                Printf.sprintf "agentctl %s (nomia %s)"
                  source.Model.policy.compiler source.policy.release
            | Error _ -> "agentctl dev")
      in
      print_endline message;
      0)

(* ------------------------------------------------------------------ *)
(* Command line                                                        *)
(* ------------------------------------------------------------------ *)

let source_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "source" ] ~docv:"DIR"
        ~doc:
          "Policy source root. Defaults to NOMIA_SOURCE, then the \
           nearest nomia/policy.yaml upward, then ./policy.yaml.")

let repo_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "repo" ] ~docv:"DIR"
        ~doc:"Repository context for its lock and trusted fragment.")

let inherit_root_flag =
  Arg.(
    value & flag
    & info [ "inherit-root" ]
        ~doc:
          "For --project targets inside an already-managed repository: render \
           only the project delta because the root supplies the organization \
           core, root fragment, skills, and client configuration.")

let project_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "project" ] ~docv:"DIR"
        ~doc:
          "Project path relative to --repo. Selects that project's reviewed \
           fragment after the repository-root fragment; sibling projects are \
           never included.")

let role_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "role" ] ~docv:"ROLE" ~doc:"Role charter to compose.")

let client_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "client" ] ~docv:"CLIENT"
        ~doc:"Client adapter (generic-agents, opencode, erde, or saguaro).")

let release_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "release" ] ~docv:"DIGEST"
        ~doc:
          "Pinned cached release digest to serve. A missing cache entry is a \
           startup error.")

let update_pin_flag =
  Arg.(
    value & flag
    & info [ "update-pin" ]
        ~doc:
          "Rewrite nomia.lock for the selected release. This is the \
           explicit reviewable re-pin operation.")

let required_client =
  Arg.(
    required
    & opt (some string) None
    & info [ "client" ] ~docv:"CLIENT"
        ~doc:"Client adapter to render (generic-agents, opencode, erde, or saguaro).")

let required_out =
  Arg.(
    required
    & opt (some string) None
    & info [ "out" ] ~docv:"DIR"
        ~doc:"Fresh directory that receives the rendered release.")

let source_argument =
  Arg.(
    value
    & pos 0 (some string) None
    & info [] ~docv:"SOURCE"
        ~doc:
          "Policy source root. Defaults to NOMIA_SOURCE, then the \
           nearest nomia/policy.yaml upward, then ./policy.yaml.")

let json_flag =
  Arg.(
    value & flag
    & info [ "json" ] ~doc:"Emit canonical JSON instead of text.")

let request_option =
  Arg.(
    value
    & opt (some string) None
    & info [ "request" ] ~docv:"FILE"
        ~doc:
          "Read one JSON enforcement request from FILE instead of stdin.")

let check_command =
  let doc = "Validate policy source for maintainers and CI." in
  Cmd.v (Cmd.info "check" ~doc)
    Term.(const check_run $ source_argument $ json_flag)

let render_command =
  let doc = "Render one client release into a fresh output directory." in
  Cmd.v (Cmd.info "render" ~doc)
    Term.(
      const render_run $ required_client $ repo_option $ project_option
      $ inherit_root_flag $ role_option $ source_option $ required_out)

let explain_command =
  let doc = "Explain the exact PolicySet that render would use." in
  Cmd.v (Cmd.info "explain" ~doc)
    Term.(
      const explain_run $ repo_option $ project_option $ inherit_root_flag
      $ role_option $ client_option $ source_option $ json_flag)

let decide_command =
  let doc =
    "Decide one normalized host action from a JSON request on stdin. Exits 0 \
     for allow, 3 for deny, 4 for ask, and 1 for a usage or policy error."
  in
  Cmd.v (Cmd.info "decide" ~doc)
    Term.(
      const decide_run $ repo_option $ project_option $ role_option
      $ client_option $ source_option $ json_flag $ request_option)

let sync_command =
  let doc =
    "Atomically install one rendered release into a repository or user-level \
     managed target. Validation and staging finish before current is swapped."
  in
  Cmd.v (Cmd.info "sync" ~doc)
    Term.(
      const sync_run $ repo_option $ project_option $ inherit_root_flag
      $ client_option $ role_option $ source_option $ update_pin_flag)

let doctor_command =
  let doc =
    "Report one managed state (current, stale, missing, unmanaged, \
     hand-edited, partial, offline-last-known-good) and enforcement coverage."
  in
  Cmd.v (Cmd.info "doctor" ~doc)
    Term.(
      const doctor_run $ repo_option $ project_option $ inherit_root_flag
      $ role_option $ client_option $ source_option $ json_flag)

let mcp_command =
  let doc =
    "Serve one pinned release read-only over JSON-RPC 2.0 on stdio. stdout \
     carries protocol responses only; diagnostics go to stderr."
  in
  Cmd.v (Cmd.info "mcp" ~doc)
    Term.(
      const mcp_run $ source_option $ repo_option $ project_option
      $ role_option $ client_option $ release_option)

let version_command =
  let doc = "Print the compiler and selected policy release." in
  Cmd.v (Cmd.info "version" ~doc) Term.(const version_run $ const ())

let main =
  Cmd.group
    (Cmd.info "agentctl" ~doc:"Compile the reviewed agent policy offline.")
    [
      check_command;
      render_command;
      explain_command;
      decide_command;
      sync_command;
      doctor_command;
      mcp_command;
      version_command;
    ]

let () = exit (Cmd.eval' main)
