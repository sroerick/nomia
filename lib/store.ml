(** Managed target layout and content-addressed release cache. *)

type target = {
  root : string;
  managed : string;
  releases : string;
  current : string;
  state_path : string;
}

type state = {
  schema : string;
  release : string;
  digest : string;
  source_digest : string;
  client : string;
  role : string option;
  compiler : string;
  entrypoints : string list;
}

type cached = {
  manifest : Yojson.Safe.t;
  explain : Yojson.Safe.t;
  artifacts : (string * string * int) list;
}

let managed_dir_name = ".nomia"
let managed_dir = managed_dir_name
let state_schema = "nomia-state/v1"

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

let trim_trailing_slash value =
  let rec loop index =
    if index > 1 && value.[index - 1] = '/' then loop (index - 1) else index
  in
  let length = loop (String.length value) in
  String.sub value 0 length

let target_of_root root =
  let root = trim_trailing_slash root in
  let managed = Filename.concat root managed_dir_name in
  {
    root;
    managed;
    releases = Filename.concat managed "releases";
    current = Filename.concat managed "current";
    state_path = Filename.concat managed "state.json";
  }

(* ------------------------------------------------------------------ *)
(* XDG locations                                                       *)
(* ------------------------------------------------------------------ *)

let home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some value when String.trim value <> "" -> value
  | _ ->
      invalid_arg
        "HOME is not set; refusing to guess a user policy or cache path"

let xdg_dir variable fallback =
  match Sys.getenv_opt variable with
  | Some value when String.trim value <> "" -> value
  | _ -> Filename.concat (home_dir ()) fallback

let client_host_root () =
  Filename.concat (xdg_dir "XDG_CONFIG_HOME" ".config") "nomia"

let client_target ~client = Filename.concat (client_host_root ()) ("clients/" ^ client)

let cache_root () =
  Filename.concat (xdg_dir "XDG_CACHE_HOME" ".cache") "nomia"

let cache_releases_root () = Filename.concat (cache_root ()) "releases"

(* ------------------------------------------------------------------ *)
(* Small filesystem helpers                                            *)
(* ------------------------------------------------------------------ *)

let file_exists path = try Sys.file_exists path with Sys_error _ -> false

let directory_exists path =
  try Sys.is_directory path with Sys_error _ -> false

let list_dir path =
  try
    Sys.readdir path |> Array.to_list
    |> List.filter (fun name -> name <> "." && name <> "..")
    |> List.sort String.compare
  with Sys_error _ -> []

let read_file path =
  try Ok (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error _ -> Error (Printf.sprintf "cannot read '%s'" (Filename.basename path))

let error_of_unix error function_name argument =
  Printf.sprintf "%s: %s %s" (Unix.error_message error) function_name
    (Filename.basename argument)

let rec mkdir_p dir =
  if dir = "" || dir = "." || dir = "/" then Ok ()
  else if file_exists dir then
    if directory_exists dir then Ok ()
    else Error (Printf.sprintf "'%s' exists and is not a directory" (Filename.basename dir))
  else
    match mkdir_p (Filename.dirname dir) with
    | Error message -> Error message
    | Ok () -> (
        try
          Unix.mkdir dir 0o755;
          Ok ()
        with
        | Unix.Unix_error (Unix.EEXIST, _, _) when directory_exists dir -> Ok ()
        | Unix.Unix_error (error, function_name, argument) ->
            Error (error_of_unix error function_name argument)
        | Sys_error message -> Error message)

let write_file ~path ~mode contents =
  match mkdir_p (Filename.dirname path) with
  | Error message -> Error message
  | Ok () -> (
      let temporary = path ^ ".part." ^ string_of_int (Unix.getpid ()) in
      try
        let descriptor =
          Unix.openfile temporary
            [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
            mode
        in
        let channel = Unix.out_channel_of_descr descriptor in
        Fun.protect
          ~finally:(fun () -> close_out_noerr channel)
          (fun () ->
            output_string channel contents;
            flush channel;
            Unix.fsync descriptor);
        Unix.chmod temporary mode;
        Unix.rename temporary path;
        Ok ()
      with
      | Sys_error message ->
          (try Sys.remove temporary with Sys_error _ -> ());
          Error message
      | Unix.Unix_error (error, function_name, argument) ->
          (try Sys.remove temporary with Sys_error _ -> ());
          Error (error_of_unix error function_name argument))

let atomic_write = write_file

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | exception Sys_error _ -> ()
  | stats -> (
      match stats.Unix.st_kind with
      | Unix.S_DIR ->
          List.iter
            (fun name -> remove_tree (Filename.concat path name))
            (list_dir path);
          (try Unix.rmdir path with Unix.Unix_error _ | Sys_error _ -> ())
      | _ -> ( try Unix.unlink path with Unix.Unix_error _ | Sys_error _ -> ()))

let read_symlink path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
  | exception Unix.Unix_error (error, function_name, argument) ->
      Error (error_of_unix error function_name argument)
  | stats -> (
      match stats.Unix.st_kind with
      | Unix.S_LNK -> (
          try Ok (Some (Unix.readlink path))
          with Unix.Unix_error (error, function_name, argument) ->
            Error (error_of_unix error function_name argument))
      | _ -> Ok None)

let release_dir target ~digest = Filename.concat target.releases digest

let staging_path target =
  Filename.concat target.releases (".tmp." ^ string_of_int (Unix.getpid ()))

let current_digest target =
  match read_symlink target.current with
  | Error message -> Error message
  | Ok None -> Ok None
  | Ok (Some link) ->
      let prefix = "releases/" in
      if starts_with ~prefix link then begin
        let digest =
          String.sub link (String.length prefix)
            (String.length link - String.length prefix)
        in
        if digest <> "" && not (String.contains digest '/') then Ok (Some digest)
        else Ok None
      end
      else Ok None

(* ------------------------------------------------------------------ *)
(* State                                                               *)
(* ------------------------------------------------------------------ *)

let option_string = function None -> `Null | Some value -> `String value
let strings values = `List (List.map (fun value -> `String value) values)

let state_to_json state =
  `Assoc
    [
      ("schema", `String state.schema);
      ("release", `String state.release);
      ("digest", `String state.digest);
      ("source_digest", `String state.source_digest);
      ("client", `String state.client);
      ("role", option_string state.role);
      ("compiler", `String state.compiler);
      ("entrypoints", strings state.entrypoints);
    ]

let state_to_string state =
  Canonical.to_string (state_to_json state) ^ "\n"

let member key = function
  | `Assoc entries -> List.assoc_opt key entries
  | _ -> None

let string_field json key =
  match member key json with
  | Some (`String value) -> Ok value
  | Some _ -> Error (Printf.sprintf "state field '%s' must be a string" key)
  | None -> Error (Printf.sprintf "state field '%s' is required" key)

let option_string_field json key =
  match member key json with
  | Some `Null | None -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> Error (Printf.sprintf "state field '%s' must be a string" key)

let string_list_field json key =
  match member key json with
  | Some (`List values) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String value :: rest -> loop (value :: acc) rest
        | _ :: _ -> Error (Printf.sprintf "state field '%s' must be a string list" key)
      in
      loop [] values
  | Some _ -> Error (Printf.sprintf "state field '%s' must be a string list" key)
  | None -> Error (Printf.sprintf "state field '%s' is required" key)

let valid_digest value =
  let prefix = "sha256:" in
  String.length value = String.length prefix + 64
  && starts_with ~prefix value
  &&
  let rec hex index =
    index >= String.length value
    ||
    let c = value.[index] in
    ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) && hex (index + 1)
  in
  hex (String.length prefix)

let safe_relative_path value =
  value <> ""
  && Filename.is_relative value
  && not (String.contains value '\\')
  && List.for_all
       (fun component -> component <> "" && component <> "." && component <> "..")
       (String.split_on_char '/' value)

let known_clients = [ "generic-agents"; "opencode"; "erde"; "saguaro" ]

let state_of_json json =
  match
    ( string_field json "schema",
      string_field json "release",
      string_field json "digest",
      string_field json "source_digest",
      string_field json "client",
      option_string_field json "role",
      string_field json "compiler",
      string_list_field json "entrypoints" )
  with
  | ( Ok schema,
      Ok release,
      Ok digest,
      Ok source_digest,
      Ok client,
      Ok role,
      Ok compiler,
      Ok entrypoints ) ->
      if schema <> state_schema then
        Error
          (Printf.sprintf "state schema must be '%s', found '%s'" state_schema
             schema)
      else if not (valid_digest digest) then
        Error "state digest must be 'sha256:' plus 64 lowercase hex characters"
      else if not (valid_digest source_digest) then
        Error
          "state source_digest must be 'sha256:' plus 64 lowercase hex characters"
      else if not (List.mem client known_clients) then
        Error (Printf.sprintf "state client '%s' is not a known adapter" client)
      else if not (List.for_all safe_relative_path entrypoints) then
        Error "state entrypoints must be safe relative paths"
      else
        Ok
          {
            schema;
            release;
            digest;
            source_digest;
            client;
            role;
            compiler;
            entrypoints;
          }
  | (Error message, _, _, _, _, _, _, _)
  | (_, Error message, _, _, _, _, _, _)
  | (_, _, Error message, _, _, _, _, _)
  | (_, _, _, Error message, _, _, _, _)
  | (_, _, _, _, Error message, _, _, _)
  | (_, _, _, _, _, Error message, _, _)
  | (_, _, _, _, _, _, Error message, _)
  | (_, _, _, _, _, _, _, Error message) ->
      Error message

let read_state target =
  if not (file_exists target.state_path) then Ok None
  else
    match read_file target.state_path with
    | Error message -> Error message
    | Ok text -> (
        match Yojson.Safe.from_string text with
        | exception Yojson.Json_error message ->
            Error (Printf.sprintf "state.json is not valid JSON: %s" message)
        | json -> (
            match state_of_json json with
            | Ok state -> Ok (Some state)
            | Error message -> Error message))

let write_state target state =
  atomic_write ~path:target.state_path ~mode:0o644 (state_to_string state)

let state_next_path target =
  Filename.concat target.managed "state.next.json"

let write_state_next target state =
  atomic_write ~path:(state_next_path target) ~mode:0o644
    (state_to_string state)

let promote_state_next target =
  try
    Sys.rename (state_next_path target) target.state_path;
    Ok ()
  with Sys_error message -> Error message

let discard_state_next target =
  try Sys.remove (state_next_path target)
  with Sys_error _ | Unix.Unix_error _ -> ()

let read_state_next target =
  let path = state_next_path target in
  if not (file_exists path) then Ok None
  else
    match read_file path with
    | Error message -> Error message
    | Ok contents -> (
        try
          match state_of_json (Yojson.Safe.from_string contents) with
          | Ok state -> Ok (Some state)
          | Error message -> Error ("invalid staged state: " ^ message)
        with Yojson.Json_error message ->
          Error (Printf.sprintf "invalid staged state: %s" message))

let effective_state target =
  match (read_state target, current_digest target, read_state_next target) with
  | Ok state, Ok (Some current), Ok (Some next) when next.digest = current -> (
      match state with
      | Some state when state.digest = current -> Ok (Some state)
      | _ -> Ok (Some next))
  | Ok state, _, _ -> Ok state
  | Error message, _, _ -> Error message

let recover_state target =
  match (current_digest target, read_state_next target) with
  | Ok (Some current), Ok (Some next) when next.digest = current ->
      promote_state_next target
  | Ok _, Ok (Some _) ->
      discard_state_next target;
      Ok ()
  | Ok _, Ok None -> Ok ()
  | Ok _, Error message -> Error message
  | Error message, _ -> Error message

(* ------------------------------------------------------------------ *)
(* Release integrity                                                   *)
(* ------------------------------------------------------------------ *)

type walked = File of string | Link of string | Directory

let rec walk_relative root relative acc =
  let directory =
    if relative = "" then root else Filename.concat root relative
  in
  match Sys.readdir directory with
  | exception Sys_error _ -> acc
  | names ->
      Array.fold_left
        (fun acc name ->
          let child = if relative = "" then name else relative ^ "/" ^ name in
          let full = Filename.concat root child in
          match Unix.lstat full with
          | exception Unix.Unix_error _ -> acc
          | stats -> (
              match stats.Unix.st_kind with
              | Unix.S_DIR -> walk_relative root child acc
              | Unix.S_LNK -> (child, Link child) :: acc
              | _ -> (child, File child) :: acc))
        acc names

let sha256_file_safe path =
  try Ok (Canonical.sha256_file path)
  with Sys_error _ ->
    Error (Printf.sprintf "cannot hash '%s'" (Filename.basename path))

let json_artifacts json =
  match member "artifacts" json with
  | Some (`List entries) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `Assoc _ as entry :: rest -> (
            match
              (member "path" entry, member "sha256" entry)
            with
            | Some (`String path), Some (`String hash) ->
                loop ((path, hash) :: acc) rest
            | _ -> Error "release manifest artifact entries need path and sha256")
        | _ :: _ -> Error "release manifest artifacts must be objects"
      in
      loop [] entries
  | _ -> Error "release manifest has no artifacts list"

let safe_relative path =
  path <> ""
  && Filename.is_relative path
  && (String.split_on_char '/' path
     |> List.for_all (fun part -> part <> "" && part <> "." && part <> ".."))

let verify_tree ~root ~digest =
  let manifest_path = Filename.concat root ".nomia/manifest.json" in
  match read_file manifest_path with
  | Error _ -> Error "release manifest is missing"
  | Ok text -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error _ -> Error "release manifest is not valid JSON"
      | manifest -> (
          match member "digest" manifest with
          | Some (`String recorded) when recorded = digest -> (
              match json_artifacts manifest with
              | Error message -> Error message
              | Ok entries ->
                  let expected = Hashtbl.create (List.length entries) in
                  List.iter
                    (fun (path, hash) -> Hashtbl.replace expected path hash)
                    entries;
                  let rec verify = function
                    | [] -> Ok ()
                    | (path, hash) :: rest ->
                        if not (safe_relative path) then
                          Error
                            (Printf.sprintf
                               "release manifest names an unsafe artifact path \
                                '%s'"
                               path)
                        else begin
                          let full = Filename.concat root path in
                          match Unix.lstat full with
                          | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                              Error
                                (Printf.sprintf
                                   "release file '%s' is missing" path)
                          | exception Unix.Unix_error _ ->
                              Error
                                (Printf.sprintf
                                   "release file '%s' is not readable" path)
                          | stats when stats.Unix.st_kind = Unix.S_LNK ->
                              Error
                                (Printf.sprintf
                                   "release file '%s' was replaced by a symlink"
                                   path)
                          | _ -> (
                              match sha256_file_safe full with
                              | Error _ ->
                                  Error
                                    (Printf.sprintf
                                       "release file '%s' is not readable" path)
                              | Ok actual when actual = hash -> verify rest
                              | Ok _ ->
                                  Error
                                    (Printf.sprintf
                                       "release file '%s' no longer matches its \
                                        manifest hash"
                                       path))
                        end
                  in
                  (match verify entries with
                  | Error message -> Error message
                  | Ok () ->
                      let walked = walk_relative root "" [] in
                      let rec check = function
                        | [] -> Ok ()
                        | (path, kind) :: rest ->
                            if path = ".nomia/manifest.json" then
                              check rest
                            else if not (Hashtbl.mem expected path) then
                              Error
                                (Printf.sprintf
                                   "release contains unexpected file '%s'" path)
                            else
                              match kind with
                              | Link _ ->
                                  Error
                                    (Printf.sprintf
                                       "release file '%s' was replaced by a \
                                        symlink"
                                       path)
                              | Directory -> check rest
                              | File _ ->
                                  if Hashtbl.mem expected path then check rest
                                  else
                                    Error
                                      (Printf.sprintf
                                         "release contains unexpected file '%s'"
                                         path)
                      in
                      let listed =
                        walked
                        |> List.filter (fun (_, kind) ->
                               match kind with File _ -> true | _ -> false)
                      in
                      (match check walked with
                      | Error message -> Error message
                      | Ok () ->
                          let expected_count = List.length entries + 1 in
                          if List.length listed <> expected_count then
                            Error
                              "release file list does not match its manifest"
                          else Ok ())))
          | Some (`String _) ->
              Error "release manifest digest does not match state.json"
          | _ -> Error "release manifest has no digest"))

let verify_release target ~digest =
  verify_tree ~root:(release_dir target ~digest) ~digest

(* ------------------------------------------------------------------ *)
(* Entrypoints and current                                             *)
(* ------------------------------------------------------------------ *)

let entrypoint_link ~entrypoint =
  let directory = Filename.dirname entrypoint in
  let depth =
    if directory = "." then 0
    else List.length (String.split_on_char '/' directory)
  in
  let base = managed_dir_name ^ "/current/" ^ entrypoint in
  if depth = 0 then base
  else String.concat "/" (List.init depth (fun _ -> "..")) ^ "/" ^ base

let atomic_symlink ~path ~link =
  let temporary = path ^ ".tmp." ^ string_of_int (Unix.getpid ()) in
  (try Unix.unlink temporary with Unix.Unix_error _ | Sys_error _ -> ());
  try
    Unix.symlink link temporary;
    Unix.rename temporary path;
    Ok ()
  with
  | Unix.Unix_error (error, function_name, argument) ->
      (try Unix.unlink temporary with Unix.Unix_error _ | Sys_error _ -> ());
      Error (error_of_unix error function_name argument)
  | Sys_error message ->
      (try Unix.unlink temporary with Unix.Unix_error _ | Sys_error _ -> ());
      Error message

let ensure_entrypoints target ~entrypoints =
  let rec loop = function
    | [] -> Ok ()
    | entrypoint :: rest ->
        let path = Filename.concat target.root entrypoint in
        let link = entrypoint_link ~entrypoint in
        let destination =
          Filename.concat target.managed
            (Filename.concat "current" entrypoint)
        in
        if not (file_exists destination) then
          Error
            (Printf.sprintf
               "rendered release does not contain entrypoint '%s'" entrypoint)
        else if file_exists path then
          match Unix.lstat path with
          | stats when stats.Unix.st_kind = Unix.S_LNK -> (
              match Unix.readlink path with
              | existing when existing = link -> loop rest
              | existing ->
                  if Filename.is_relative existing
                     && contains_sub existing (managed_dir_name ^ "/current/")
                  then
                    (* A managed link with stale spelling is safe to repair. *)
                    match atomic_symlink ~path ~link with
                    | Ok () -> loop rest
                    | Error message -> Error message
                  else
                    Error
                      (Printf.sprintf
                         "entrypoint '%s' is a symlink that sync did not \
                          create; refusing to overwrite it"
                         entrypoint))
          | _ ->
              Error
                (Printf.sprintf
                   "entrypoint '%s' already exists and is not a managed \
                    symlink; move or remove it before syncing"
                   entrypoint)
        else
          match mkdir_p (Filename.dirname path) with
          | Error message -> Error message
          | Ok () -> (
              match atomic_symlink ~path ~link with
              | Ok () -> loop rest
              | Error message -> Error message)
  in
  loop entrypoints

let swap_current target ~digest =
  if not (directory_exists (release_dir target ~digest)) then
    Error (Printf.sprintf "release '%s' is not installed" digest)
  else
    let temporary =
      target.current ^ ".tmp." ^ string_of_int (Unix.getpid ())
    in
    (try Unix.unlink temporary with Unix.Unix_error _ | Sys_error _ -> ());
    try
      Unix.symlink ("releases/" ^ digest) temporary;
      Unix.rename temporary target.current;
      (try
         let descriptor = Unix.openfile target.managed [ Unix.O_RDONLY ] 0 in
         Fun.protect
           ~finally:(fun () -> Unix.close descriptor)
           (fun () -> Unix.fsync descriptor)
       with Unix.Unix_error _ -> ());
      Ok ()
    with
    | Unix.Unix_error (error, function_name, argument) ->
        (try Unix.unlink temporary with Unix.Unix_error _ | Sys_error _ -> ());
        Error (error_of_unix error function_name argument)
    | Sys_error message ->
        (try Unix.unlink temporary with Unix.Unix_error _ | Sys_error _ -> ());
        Error message

let prune_releases target ~current ~keep =
  let entries =
    list_dir target.releases
    |> List.filter (fun name -> name <> current && name <> "" && name.[0] <> '.')
    |> List.filter (fun name ->
           directory_exists (Filename.concat target.releases name))
  in
  let mtime name =
    try (Unix.lstat (Filename.concat target.releases name)).Unix.st_mtime
    with Unix.Unix_error _ -> 0.
  in
  let sorted =
    List.sort
      (fun left right ->
        let by_time = compare (mtime right) (mtime left) in
        if by_time <> 0 then by_time else String.compare left right)
      entries
  in
  let rec drop index = function
    | [] -> []
    | _ :: rest when index >= keep -> rest
    | _ :: rest -> drop (index + 1) rest
  in
  let obsolete = drop 0 sorted in
  List.iter
    (fun name -> remove_tree (Filename.concat target.releases name))
    obsolete;
  Ok ()

(* ------------------------------------------------------------------ *)
(* Content-addressed cache                                             *)
(* ------------------------------------------------------------------ *)

let cache_digests () =
  list_dir (cache_releases_root ())
  |> List.filter (fun name -> name <> "" && name.[0] <> '.')

let cache_dir ~digest = Filename.concat (cache_releases_root ()) digest

let cache_manifest ~digest =
  let path = Filename.concat (cache_dir ~digest) "manifest.json" in
  match read_file path with
  | Error _ -> Error "cached release manifest is missing"
  | Ok text -> (
      match Yojson.Safe.from_string text with
      | exception Yojson.Json_error _ ->
          Error "cached release manifest is not valid JSON"
      | json -> (
          match member "digest" json with
          | Some (`String recorded) when recorded = digest -> Ok json
          | _ ->
              Error "cached release manifest digest does not match its path"))

let cache_release ~digest ~manifest ~explain ~artifacts =
  let directory = cache_dir ~digest in
  if directory_exists directory then Ok ()
  else
    match mkdir_p (cache_releases_root ()) with
    | Error message -> Error message
    | Ok () ->
        let stage =
          Filename.concat (cache_releases_root ())
            (".tmp." ^ string_of_int (Unix.getpid ()))
        in
        remove_tree stage;
        let cleanup () = remove_tree stage in
        let result =
          match mkdir_p stage with
          | Error message -> Error message
          | Ok () -> (
              match
                write_file
                  ~path:(Filename.concat stage "manifest.json")
                  ~mode:0o644
                  (Canonical.to_string manifest ^ "\n")
              with
              | Error message -> Error message
              | Ok () -> (
                  match
                    write_file
                      ~path:(Filename.concat stage "explain.json")
                      ~mode:0o644
                      (Canonical.to_string explain ^ "\n")
                  with
                  | Error message -> Error message
                  | Ok () ->
                      let rec write = function
                        | [] -> Ok ()
                        | (path, contents, mode) :: rest ->
                            if not (safe_relative path) then
                              Error
                                (Printf.sprintf
                                   "cached artifact path '%s' is unsafe" path)
                            else
                              match
                                write_file
                                  ~path:
                                    (Filename.concat stage
                                       (Filename.concat "artifacts" path))
                                  ~mode contents
                              with
                              | Error message -> Error message
                              | Ok () -> write rest
                      in
                      write artifacts))
        in
        (match result with
        | Error message ->
            cleanup ();
            Error message
        | Ok () ->
            if directory_exists directory then begin
              cleanup ();
              Ok ()
            end
            else
              try
                Unix.rename stage directory;
                Ok ()
              with Unix.Unix_error (error, function_name, argument) ->
                cleanup ();
                if directory_exists directory then Ok ()
                else Error (error_of_unix error function_name argument))

let load_cached ~digest =
  match cache_manifest ~digest with
  | Error message -> Error message
  | Ok manifest -> (
      match json_artifacts manifest with
      | Error message -> Error message
      | Ok entries ->
          let explain_path = Filename.concat (cache_dir ~digest) "explain.json" in
          match read_file explain_path with
          | Error _ -> Error "cached release explain.json is missing"
          | Ok explain_text -> (
              match Yojson.Safe.from_string explain_text with
              | exception Yojson.Json_error _ ->
                  Error "cached release explain.json is not valid JSON"
              | explain ->
                  let rec load acc = function
                    | [] -> Ok (List.rev acc)
                    | (path, hash) :: rest ->
                        if not (safe_relative path) then
                          Error
                            (Printf.sprintf
                               "cached artifact path '%s' is unsafe" path)
                        else
                          let full =
                            Filename.concat (cache_dir ~digest)
                              (Filename.concat "artifacts" path)
                          in
                          match Unix.lstat full with
                          | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
                              Error
                                (Printf.sprintf "cached artifact '%s' is missing"
                                   path)
                          | exception Unix.Unix_error _ ->
                              Error
                                (Printf.sprintf
                                   "cached artifact '%s' is not readable" path)
                          | stats when stats.Unix.st_kind = Unix.S_LNK ->
                              Error
                                (Printf.sprintf
                                   "cached artifact '%s' is a symlink" path)
                          | stats -> (
                              match sha256_file_safe full with
                              | Error _ ->
                                  Error
                                    (Printf.sprintf
                                       "cached artifact '%s' is not readable"
                                       path)
                              | Ok actual when actual = hash -> (
                                  match read_file full with
                                  | Error _ ->
                                      Error
                                        (Printf.sprintf
                                           "cached artifact '%s' is not readable"
                                           path)
                                  | Ok contents ->
                                      let mode = stats.Unix.st_perm land 0o777 in
                                      load ((path, contents, mode) :: acc) rest)
                              | Ok _ ->
                                  Error
                                    (Printf.sprintf
                                       "cached artifact '%s' no longer matches \
                                        its digest"
                                       path))
                  in
                  (match load [] entries with
                  | Error message -> Error message
                  | Ok artifacts ->
                      let explain_bytes = Canonical.to_string explain ^ "\n" in
                      let recorded_explain =
                        List.find_opt
                          (fun (path, _, _) -> path = ".nomia/explain.json")
                          artifacts
                      in
                      (match recorded_explain with
                      | Some (_, contents, _) when contents <> explain_bytes ->
                          Error
                            "cached explain.json does not match the cached \
                             explain artifact"
                      | _ ->
                          Ok { manifest; explain; artifacts }))))

(* Installed-target fallback for offline read-only adapters. The release tree
   is verified before any file is read, so a hand-edited install is rejected
   rather than served. *)
let read_json_file ~label path =
  match read_file path with
  | Error _ -> Error (Printf.sprintf "%s is unreadable" label)
  | Ok contents -> (
      try Ok (Yojson.Safe.from_string contents)
      with Yojson.Json_error message ->
        Error (Printf.sprintf "invalid JSON in %s: %s" label message))

let rec installed_files directory relative =
  list_dir directory
  |> List.concat_map (fun name ->
         let path = Filename.concat directory name in
         let rel = if relative = "" then name else relative ^ "/" ^ name in
         match Unix.lstat path with
         | stats when stats.Unix.st_kind = Unix.S_DIR ->
             installed_files path rel
         | stats when stats.Unix.st_kind = Unix.S_REG ->
             [ (rel, path, stats.Unix.st_perm land 0o777) ]
         | _ -> []
         | exception Unix.Unix_error _ -> [])

let load_installed ~root ~digest =
  let target = target_of_root root in
  match verify_release target ~digest with
  | Error message -> Error message
  | Ok () ->
      let directory = release_dir target ~digest in
      (match
         ( read_json_file ~label:".nomia/manifest.json"
             (Filename.concat directory ".nomia/manifest.json"),
           read_json_file ~label:".nomia/explain.json"
             (Filename.concat directory ".nomia/explain.json") )
       with
      | Ok manifest, Ok explain ->
          let rec read_all acc = function
            | [] -> Ok (List.rev acc)
            | (rel, path, mode) :: rest -> (
                match read_file path with
                | Ok contents -> read_all ((rel, contents, mode) :: acc) rest
                | Error message ->
                    Error
                      (Printf.sprintf "cannot read installed artifact %s: %s"
                         rel message))
          in
          let files =
            installed_files directory ""
            |> List.filter (fun (rel, _, _) -> rel <> ".nomia/manifest.json")
            |> List.sort (fun (left, _, _) (right, _, _) ->
                   String.compare left right)
          in
          (match read_all [] files with
          | Error message -> Error message
          | Ok artifacts -> Ok { manifest; explain; artifacts })
      | Error message, _ | _, Error message -> Error message)
