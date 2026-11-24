(** Strict parsing and deterministic spelling of a repository lock. *)

type t = {
  schema : string;
  release : string;
  digest : string option;  (** Legacy exact-selection pin, when present. *)
  source_digest : string option;  (** Organization source tree pin. *)
  adapter_schema : int;
  compiler : string;
  role : string option;
  client : string option;
  fragment : string option;  (** Optional root fragment override. *)
  source : string;
}

let schema = "nomia-lock/v2"
let lock_file = "nomia.lock"

let diag code path line message : Model.diagnostic =
  { Model.code; path; line; message }

let starts_with ~prefix value =
  let length = String.length prefix in
  String.length value >= length && String.sub value 0 length = prefix

let strip_prefix ~prefix value =
  if starts_with ~prefix value then
    String.sub value (String.length prefix)
      (String.length value - String.length prefix)
  else value

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

let normalize diagnostics =
  List.sort_uniq
    (fun left right ->
      let order = compare_diagnostic left right in
      if order <> 0 then order
      else String.compare left.Model.message right.Model.message)
    diagnostics
  |> List.sort compare_diagnostic

let yaml_diagnostic path message =
  let prefix = path ^ ":" in
  let rest = strip_prefix ~prefix message in
  match String.index_opt rest ':' with
  | Some index ->
      let line_text = String.sub rest 0 index in
      let message =
        String.sub rest (index + 1) (String.length rest - index - 1)
        |> String.trim
      in
      let line =
        match int_of_string_opt (String.trim line_text) with
        | Some number -> Some number
        | None -> None
      in
      diag "yaml.parse" path line message
  | None -> diag "yaml.parse" path None message

let known_fields =
  [
    "schema";
    "release";
    "digest";
    "source_digest";
    "adapter_schema";
    "compiler";
    "role";
    "client";
    "fragment";
  ]

let parse_tree ~label tree =
  match tree with
  | Yaml_lite.Null ->
      Error
        [
          diag "lock.shape" label None
            "nomia.lock must be a non-empty mapping";
        ]
  | Yaml_lite.Map entries ->
      let diagnostics = ref [] in
      let add code line message =
        diagnostics := diag code label line message :: !diagnostics
      in
      List.iter
        (fun (key, _) ->
          if not (List.mem key known_fields) then
            add "lock.unknown-field" (Some 1)
              (Printf.sprintf "unknown lock field '%s'" key))
        entries;
      let string_field key =
        match Yaml_lite.get key tree with
        | Some (Yaml_lite.String value) -> value
        | Some Yaml_lite.Null | None -> ""
        | Some _ ->
            add "yaml.type" (Some 1)
              (Printf.sprintf "lock field '%s' must be a string" key);
            ""
      in
      let present_string key =
        match Yaml_lite.get key tree with
        | None | Some Yaml_lite.Null -> None
        | Some (Yaml_lite.String value) ->
            if String.trim value = "" then None else Some value
        | Some _ ->
            add "yaml.type" (Some 1)
              (Printf.sprintf "lock field '%s' must be a string" key);
            None
      in
      let int_field key default =
        match Yaml_lite.get key tree with
        | Some (Yaml_lite.Int value) -> value
        | None | Some Yaml_lite.Null -> default
        | Some _ ->
            add "yaml.type" (Some 1)
              (Printf.sprintf "lock field '%s' must be an integer" key);
            default
      in
      let lock_schema = string_field "schema" in
      let release = string_field "release" in
      let digest = present_string "digest" in
      let source_digest = present_string "source_digest" in
      let adapter_schema = int_field "adapter_schema" 0 in
      let compiler = string_field "compiler" in
      if lock_schema = "" then
        add "lock.schema" (Some 1) "lock field 'schema' is required"
      else if lock_schema <> schema then
        add "lock.schema" (Some 1)
          (Printf.sprintf "lock schema must be '%s', found '%s'" schema
             lock_schema);
      if release = "" then
        add "lock.release" (Some 1) "lock field 'release' is required";
      let valid_digest key field value =
        let prefix = "sha256:" in
        let valid =
          String.length value = String.length prefix + 64
          && String.sub value 0 (String.length prefix) = prefix
          &&
          let rec hex index =
            index >= String.length value
            ||
            let c = value.[index] in
            ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
            && hex (index + 1)
          in
          hex (String.length prefix)
        in
        if not valid then
          add key (Some 1)
            (Printf.sprintf
               "lock field '%s' must be 'sha256:' plus 64 lowercase hex \
                characters"
               field)
      in
      if source_digest = None then
        add "lock.source-digest" (Some 1)
          "lock field 'source_digest' is required"
      else
        Option.iter
          (valid_digest "lock.source-digest" "source_digest")
          source_digest;
      Option.iter (valid_digest "lock.digest" "digest") digest;
      if adapter_schema <> 1 then
        add "lock.adapter-schema" (Some 1)
          "lock field 'adapter_schema' must be 1 for this compiler";
      if compiler = "" then
        add "lock.compiler" (Some 1) "lock field 'compiler' is required";
      (match !diagnostics with
      | [] ->
          Ok
            {
              schema = lock_schema;
              release;
              digest;
              source_digest;
              adapter_schema;
              compiler;
              role = present_string "role";
              client = present_string "client";
              fragment = present_string "fragment";
              source = label;
            }
      | diagnostics -> Error (normalize diagnostics))
  | _ ->
      Error
        [
          diag "lock.shape" label None
            "nomia.lock must be a YAML mapping";
        ]

let parse_file ~read_path ~label =
  match
    try Ok (In_channel.with_open_bin read_path In_channel.input_all)
    with Sys_error _ -> Error ()
  with
  | Error () ->
      Error [ diag "source.read" label None "cannot read lock file" ]
  | Ok text -> (
      match Yaml_lite.parse_string ~source:label text with
      | Error message -> Error [ yaml_diagnostic label message ]
      | Ok tree -> parse_tree ~label tree)

let parse ~path = parse_file ~read_path:path ~label:path

let find_in_repo ~repo =
  let read_path = Filename.concat repo lock_file in
  if not (Sys.file_exists read_path) then Ok None
  else
    match parse_file ~read_path ~label:lock_file with
    | Ok lock -> Ok (Some lock)
    | Error diagnostics -> Error diagnostics

let yaml_scalar value =
  let needs_quotes =
    value = ""
    || String.for_all
         (fun character ->
           match character with
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '/' | '-' | '_' | ':'
             ->
               true
           | _ -> false)
         value
    |> not
  in
  if not needs_quotes then value
  else
    let escaped =
      String.concat "" (List.map (fun character ->
          match character with
          | '"' -> "\\\""
          | '\\' -> "\\\\"
          | '\n' -> "\\n"
          | character -> String.make 1 character)
        (List.init (String.length value) (String.get value)))
    in
    "\"" ^ escaped ^ "\""

let to_yaml_string lock =
  let buffer = Buffer.create 256 in
  let line key value =
    Buffer.add_string buffer (Printf.sprintf "%s: %s\n" key (yaml_scalar value))
  in
  line "schema" lock.schema;
  line "release" lock.release;
  Option.iter (line "digest") lock.digest;
  Option.iter (line "source_digest") lock.source_digest;
  Buffer.add_string buffer
    (Printf.sprintf "adapter_schema: %d\n" lock.adapter_schema);
  line "compiler" lock.compiler;
  Option.iter (line "role") lock.role;
  Option.iter (line "client") lock.client;
  Option.iter (line "fragment") lock.fragment;
  Buffer.contents buffer
