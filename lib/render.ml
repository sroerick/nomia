(** Deterministic rendering of one resolved PolicySet. *)

open Model

type artifact = { path : string; contents : string; mode : int }

let supported_clients = [ "generic-agents"; "opencode"; "erde"; "saguaro" ]

let file_mode = 0o644
let script_mode = 0o755

let diagnostic code path message : Model.diagnostic =
  { code; path; line = None; message }

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

let sort_diagnostics diagnostics =
  List.sort
    (fun left right ->
      let by_position = compare_diagnostic left right in
      if by_position <> 0 then by_position
      else String.compare left.Model.message right.Model.message)
    diagnostics

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | contents -> Some contents
  | exception Sys_error _ -> None

let is_script relative_path =
  String.split_on_char '/' relative_path
  |> List.exists (fun component -> component = "scripts")
  || Filename.check_suffix relative_path ".sh"

let skill_mode relative_path =
  if is_script relative_path then script_mode else file_mode

(* ------------------------------------------------------------------ *)
(* Skill installation                                                  *)
(* ------------------------------------------------------------------ *)

let skill_artifacts (policy_set : Compose.t) =
  let source = policy_set.Compose.source in
  let install_dir = policy_set.Compose.adapter.Model.skills.dir in
  let diagnostics = ref [] in
  let artifacts =
    policy_set.Compose.skills
    |> List.concat_map (fun (skill : Model.skill) ->
           skill.files
           |> List.filter_map (fun (relative, expected_hash) ->
                  let source_path =
                    Filename.concat source.Model.root
                      (Filename.concat (Filename.concat "skills" skill.name)
                         relative)
                  in
                  let relative_display =
                    "skills/" ^ skill.name ^ "/" ^ relative
                  in
                  match read_file source_path with
                  | None ->
                      diagnostics :=
                        diagnostic "render.skill.read" relative_display
                          (Printf.sprintf
                             "cannot read skill file '%s' while rendering"
                             relative_display)
                        :: !diagnostics;
                      None
                  | Some contents ->
                      let actual_hash = Source.sha256_hex contents in
                      if actual_hash <> expected_hash then begin
                        diagnostics :=
                          diagnostic "render.skill.hash" relative_display
                            (Printf.sprintf
                               "skill file '%s' changed after it was loaded: \
                                expected sha256 %s, found %s"
                               relative_display expected_hash actual_hash)
                          :: !diagnostics;
                        None
                      end
                      else
                        Some
                          {
                            path =
                              install_dir ^ "/" ^ skill.name ^ "/" ^ relative;
                            contents;
                            mode = skill_mode relative;
                          }))
  in
  (artifacts, !diagnostics)

(* ------------------------------------------------------------------ *)
(* Rendering                                                           *)
(* ------------------------------------------------------------------ *)

let render (policy_set : Compose.t) ~client =
  if not (List.mem client supported_clients) then
    Error
      [
        diagnostic "render.client.unknown" "adapters"
          (Printf.sprintf
             "client '%s' has no renderer in this agentctl build; supported \
              clients: %s"
             client
             (String.concat ", " supported_clients));
      ]
  else if client <> policy_set.Compose.adapter.Model.id then
    Error
      [
        diagnostic "render.client.mismatch" "adapters"
          (Printf.sprintf
             "client '%s' does not match the composed adapter '%s'"
             client policy_set.Compose.adapter.id);
      ]
  else if client = "saguaro" && policy_set.Compose.role <> None then
    Error
      [
        diagnostic "render.role.unsupported" "adapters"
          "the Saguaro persona renderer does not carry a role charter; render \
           without --role and keep role behavior in the Saguaro configuration";
      ]
  else if
    policy_set.Compose.inherit_root
    && client <> "generic-agents" && client <> "opencode"
  then
    Error
      [
        diagnostic "render.inherit-root.unsupported" "adapters"
          "a project overlay is supported only for generic AGENTS.md and \
           OpenCode targets; the managed repository root supplies shared \
           skills and configuration";
      ]
  else
    let explain =
      Canonical.to_string (Compose.explain_json policy_set) ^ "\n"
    in
    let skills, skill_diagnostics =
      if policy_set.Compose.inherit_root then ([], [])
      else skill_artifacts policy_set
    in
    let client_result =
      match client with
      | "generic-agents" ->
          Ok
            [
              {
                path = "AGENTS.md";
                contents = Render_generic.agents_md policy_set;
                mode = file_mode;
              };
            ]
      | "opencode" ->
          let agents =
            {
              path = "AGENTS.md";
              contents = Render_generic.agents_md policy_set;
              mode = file_mode;
            }
          in
          if policy_set.Compose.inherit_root then Ok [ agents ]
          else
            Ok
              [
                agents;
                {
                  path = "opencode.json";
                  contents = Render_opencode.config policy_set;
                  mode = file_mode;
                };
              ]
      | "erde" -> (
          match Render_erde.memory_contract policy_set with
          | Error diagnostics -> Error diagnostics
          | Ok memory_contract ->
              let framework =
                Render_erde.framework policy_set ~memory_contract
              in
              Ok
                [
                  {
                    path = "org-core.md";
                    contents = Render_erde.org_core policy_set;
                    mode = file_mode;
                  };
                  {
                    path = "framework.md";
                    contents = framework;
                    mode = file_mode;
                  };
                  {
                    path = "policy.json";
                    contents =
                      Render_erde.policy_json policy_set
                        ~memory_contract_text:memory_contract
                        ~framework_text:framework;
                    mode = file_mode;
                  };
                ])
      | "saguaro" -> (
          match Render_saguaro.template policy_set with
          | Error diagnostics -> Error diagnostics
          | Ok template_text -> (
              match Render_saguaro.persona policy_set ~template_text with
              | Error diagnostics -> Error diagnostics
              | Ok persona_text ->
                  Ok
                    [
                      {
                        path = "persona.md";
                        contents = persona_text;
                        mode = file_mode;
                      };
                      {
                        path = "policy.json";
                        contents =
                          Render_saguaro.policy_json policy_set ~template_text
                            ~persona_text;
                        mode = file_mode;
                      };
                    ]))
      | _ -> assert false
    in
    match client_result with
    | Error diagnostics ->
        Error (sort_diagnostics (skill_diagnostics @ diagnostics))
    | Ok client_artifacts ->
        if skill_diagnostics <> [] then
          Error (sort_diagnostics skill_diagnostics)
        else
          Ok
            (client_artifacts
            @ [
                {
                  path = ".nomia/explain.json";
                  contents = explain;
                  mode = file_mode;
                };
              ]
            @ skills)

(* ------------------------------------------------------------------ *)
(* Filesystem effects                                                  *)
(* ------------------------------------------------------------------ *)

let check_out ~out =
  if String.trim out = "" then Error "output directory must not be empty"
  else
    try
      if not (Sys.file_exists out) then Ok ()
      else if not (Sys.is_directory out) then
        Error
          (Printf.sprintf
             "output path '%s' already exists and is not a directory" out)
      else
        match Sys.readdir out with
        | [||] -> Ok ()
        | _ ->
            Error
              (Printf.sprintf
                 "output directory '%s' is not empty; render needs a fresh \
                  directory"
                 out)
    with Sys_error message -> Error message

let safe_relative_path path =
  path <> ""
  && not (String.contains path '\000')
  && Filename.is_relative path
  && (String.split_on_char '/' path
     |> List.for_all (fun component ->
            component <> "" && component <> "." && component <> ".."))

let validate_artifacts artifacts =
  let seen = Hashtbl.create 16 in
  let rec loop = function
    | [] -> Ok ()
    | (artifact : artifact) :: rest ->
        if not (safe_relative_path artifact.path) then
          Error
            (Printf.sprintf
               "artifact path '%s' must be a non-empty relative path inside \
                the output directory"
               artifact.path)
        else if Hashtbl.mem seen artifact.path then
          Error (Printf.sprintf "duplicate artifact path '%s'" artifact.path)
        else begin
          Hashtbl.add seen artifact.path ();
          loop rest
        end
  in
  loop artifacts

let rec mkdir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" then
    if Sys.file_exists dir then begin
      if not (Sys.is_directory dir) then
        invalid_arg (Printf.sprintf "%s exists and is not a directory" dir)
    end
    else begin
      mkdir_p (Filename.dirname dir);
      Unix.mkdir dir 0o755
    end

let write_file path mode contents =
  let descriptor =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] mode
  in
  let channel = Unix.out_channel_of_descr descriptor in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents);
  Unix.chmod path mode

let write ~out artifacts =
  match check_out ~out with
  | Error message -> Error message
  | Ok () -> (
      match validate_artifacts artifacts with
      | Error message -> Error message
      | Ok () -> (
          try
            mkdir_p out;
            List.iter
              (fun (artifact : artifact) ->
                let path = Filename.concat out artifact.path in
                mkdir_p (Filename.dirname path);
                write_file path artifact.mode artifact.contents)
              artifacts;
            Ok ()
          with
          | Sys_error message -> Error message
          | Invalid_argument message -> Error message
          | Unix.Unix_error (error, function_name, argument) ->
              Error
                (Printf.sprintf "%s: %s %s" (Unix.error_message error)
                   function_name argument)))
