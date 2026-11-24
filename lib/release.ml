(** Immutable, content-addressed release manifests. *)

open Model

let schema = "nomia-release/v1"
let source_schema = "nomia-source/v1"

let strength_name = function Must -> "must" | Should -> "should" | May -> "may"

let kind_name = function
  | Instruction -> "instruction"
  | Procedure -> "procedure"
  | Knowledge -> "knowledge"
  | Capability -> "capability"

let class_name = function
  | Advisory -> "advisory"
  | Gate -> "gate"
  | Consent -> "consent"
  | Prohibition -> "prohibition"

let strings values = `List (List.map (fun value -> `String value) values)
let option_string = function None -> `Null | Some value -> `String value

let sort_files files =
  List.sort_uniq
    (fun (left, _) (right, _) -> String.compare left right)
    files

let source_hash files path = List.assoc_opt path files |> Option.value ~default:""

let file_list_json files =
  `List
    (List.map
       (fun (path, hash) ->
         `Assoc [ ("path", `String path); ("sha256", `String hash) ])
       files)

let guard_json (guard : guard) =
  `Assoc
    [
      ("action", `String guard.action);
      ("paths", strings guard.paths);
      ("hosts", strings guard.hosts);
    ]

let enforcement_json (enforcement : enforcement) =
  `Assoc
    [
      ("class", `String (class_name enforcement.cls));
      ("controls", strings enforcement.controls);
    ]

let rule_json files (rule : rule) =
  `Assoc
    [
      ("id", `String rule.id);
      ("revision", `Int rule.revision);
      ("owner", `String rule.owner);
      ("strength", `String (strength_name rule.strength));
      ("scope", strings rule.scope);
      ("final", `Bool rule.final);
      ("kind", `String (kind_name rule.kind));
      ( "enforcement",
        match rule.enforcement with
        | None -> `Null
        | Some enforcement -> enforcement_json enforcement );
      ("actions", strings rule.actions);
      ("guards", `List (List.map guard_json rule.guards));
      ("key", option_string rule.key);
      ("value", option_string rule.value);
      ("supersedes", option_string rule.supersedes);
      ("body_sha256", `String (Canonical.sha256_hex rule.body));
      ("source", `String rule.source);
    ]

let skill_json (skill : skill) =
  let files =
    List.sort
      (fun (left, _) (right, _) -> String.compare left right)
      skill.files
  in
  `Assoc
    [
      ("name", `String skill.name);
      ("files", file_list_json files);
    ]

let role_json files (role : role) =
  `Assoc
    [
      ("name", `String role.name);
      ("source", `String role.source);
      ("source_sha256", `String (source_hash files role.source));
      ("charter_sha256", `String (Canonical.sha256_hex role.charter));
    ]

let adapter_json files (adapter : adapter) =
  let capabilities =
    `Assoc
      [
        ( "instructions",
          `Assoc
            [
              ("file", `String adapter.instructions.file);
              ("discovery", strings adapter.instructions.discovery);
              ("includes", `Bool adapter.instructions.includes);
            ] );
        ( "skills",
          `Assoc
            [
              ("dir", `String adapter.skills.dir);
              ("auto_discovery", `Bool adapter.skills.auto_discovery);
              ("explicit_reference", `Bool adapter.skills.explicit_reference);
            ] );
        ("roles", `Bool adapter.roles);
        ("mcp", `Bool adapter.mcp);
        ("hooks", `Bool adapter.hooks);
        ("permissions", `Bool adapter.permissions);
        ("atomic_output", `Bool adapter.atomic_output);
        ( "controls",
          `Assoc
            (List.map
               (fun (cls, controls) -> (cls, strings controls))
               adapter.controls) );
        ("unsupported", strings adapter.unsupported);
      ]
  in
  `Assoc
    [
      ("id", `String adapter.id);
      ("version", `Int adapter.version);
      ("source", `String adapter.source);
      ("source_sha256", `String (source_hash files adapter.source));
      ("capabilities", capabilities);
    ]

let compare_rule (left : rule) (right : rule) =
  let by_id = String.compare left.id right.id in
  if by_id <> 0 then by_id
  else
    let by_revision = Int.compare left.revision right.revision in
    if by_revision <> 0 then by_revision
    else String.compare left.source right.source

let compare_skill (left : skill) (right : skill) =
  let by_name = String.compare left.name right.name in
  if by_name <> 0 then by_name else String.compare left.source right.source

let compare_role (left : role) (right : role) =
  let by_name = String.compare left.name right.name in
  if by_name <> 0 then by_name else String.compare left.source right.source

let compare_adapter (left : adapter) (right : adapter) =
  let by_id = String.compare left.id right.id in
  if by_id <> 0 then by_id else String.compare left.source right.source

let sorted_exceptions source exceptions =
  let ids =
    match exceptions with
    | Some ids -> ids
    | None -> List.map (fun (exception_ : exception_) -> exception_.id) source.exceptions
  in
  List.sort_uniq String.compare ids

let artifact_json (path, contents) =
  if path = "" || path.[0] = '/' then
    invalid_arg
      "Release.build: artifact paths must be non-empty and relative to the output root";
  `Assoc [ ("path", `String path); ("sha256", `String (Canonical.sha256_hex contents)) ]

type t = { manifest : Yojson.Safe.t; digest : string }

let fragments_json fragments =
  `List
    (List.map
       (fun (path, digest) ->
         `Assoc [ ("path", `String path); ("digest", `String digest) ])
       fragments)

let build ?client ?role ?project ?(inherit_root = false) ?(fragments = [])
    ?exceptions ?(artifacts = []) source =
  let files = sort_files source.files in
  let rules = List.sort compare_rule source.rules in
  let skills = List.sort compare_skill source.skills in
  let roles = List.sort compare_role source.roles in
  let adapters = List.sort compare_adapter source.adapters in
  let exception_ids = sorted_exceptions source exceptions in
  let artifacts =
    artifacts
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.map artifact_json
  in

  (* The digest covers inputs only. Artifact bytes and the manifest's own
     [digest] field are deliberately absent, so writing a header that names
     the digest never changes the digest. *)
  let inherit_fields =
    if inherit_root then [ ("inherits_root", `Bool true) ] else []
  in
  let digest_input =
    `Assoc
      ([
         ("schema", `String schema);
         ("compiler", `String source.policy.compiler);
         ("release", `String source.policy.release);
         ("source_files", file_list_json files);
         ( "adapters",
           `List
             (List.map
                (fun (adapter : adapter) ->
                  `Assoc
                    [
                      ("id", `String adapter.id);
                      ("version", `Int adapter.version);
                    ])
                adapters) );
         ("client", option_string client);
         ("role", option_string role);
         ("project", option_string project);
         ("fragments", fragments_json fragments);
         ("exceptions", strings exception_ids);
       ]
      @ inherit_fields)
  in
  let digest = Canonical.manifest_digest digest_input in
  let manifest =
    `Assoc
      ([
         ("schema", `String schema);
         ("compiler", `String source.policy.compiler);
         ("release", `String source.policy.release);
         ("client", option_string client);
         ("role", option_string role);
         ("project", option_string project);
         ("fragments", fragments_json fragments);
         ("source_files", file_list_json files);
         ("rules", `List (List.map (rule_json files) rules));
         ("skills", `List (List.map skill_json skills));
         ("roles", `List (List.map (role_json files) roles));
         ("adapters", `List (List.map (adapter_json files) adapters));
         ("exceptions", strings exception_ids);
         ("artifacts", `List artifacts);
         ("digest", `String digest);
       ]
      @ inherit_fields)
  in
  { manifest; digest }

let manifest_json manifest = manifest.manifest
let to_string manifest = Canonical.to_string manifest.manifest
let digest manifest = manifest.digest

let source_digest source =
  let files = sort_files source.files in
  Canonical.manifest_digest
    (`Assoc
      [
        ("schema", `String source_schema);
        ("files", file_list_json files);
      ])

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

let write_manifest ~dir manifest =
  try
    mkdir_p dir;
    let path = Filename.concat dir "manifest.json" in
    let temporary = path ^ ".tmp" in
    let channel = open_out_bin temporary in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () ->
        output_string channel (to_string manifest);
        output_char channel '\n');
    Sys.rename temporary path;
    Ok ()
  with
  | Sys_error message -> Error message
  | Invalid_argument message -> Error message
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (Printf.sprintf "%s: %s %s" (Unix.error_message error) function_name
           argument)
