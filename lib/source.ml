(** Deterministic loading and validation of one policy source tree.

    [load] performs the filesystem work and the schema-shape checks that need
    the raw YAML mapping (unknown fields, value types, enum spellings).
    [validate] is pure over the loaded model and checks the semantic
    invariants: ownership, references, precedence, conflicts, budgets,
    secrets, and exception expiry. [load_and_validate] composes both.

    All diagnostic paths are relative to [root]. No absolute machine path is
    ever placed in a diagnostic. *)

open Model
open Yaml_lite

let diag code path line message : diagnostic = { code; path; line; message }

(* ------------------------------------------------------------------ *)
(* Small pure helpers                                                  *)
(* ------------------------------------------------------------------ *)

let contains_sub hay needle =
  let lh = String.length hay and ln = String.length needle in
  if ln = 0 then true
  else
    let rec loop i =
      if i + ln > lh then false
      else if String.sub hay i ln = needle then true
      else loop (i + 1)
    in
    loop 0

let starts_with ~prefix s =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix

let ends_with ~suffix s =
  let ls = String.length s and lx = String.length suffix in
  ls >= lx && String.sub s (ls - lx) lx = suffix

let strip_prefix ~prefix s =
  if starts_with ~prefix s then
    String.sub s (String.length prefix) (String.length s - String.length prefix)
  else s

let is_lower = function 'a' .. 'z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_lower_alnum c = is_lower c || is_digit c
let is_upper_alnum c = (c >= 'A' && c <= 'Z') || is_digit c
let is_word_char c = is_lower_alnum c || (c >= 'A' && c <= 'Z') || c = '_'

let is_kebab_case s =
  let n = String.length s in
  n > 0 && is_lower s.[0]
  &&
  let rec loop i prev_hyphen =
    if i = n then not prev_hyphen
    else
      let c = s.[i] in
      if c = '-' then if prev_hyphen then false else loop (i + 1) true
      else if is_lower_alnum c then loop (i + 1) false
      else false
  in
  loop 1 false

let all_digits s = String.length s > 0 && String.for_all is_digit s

let valid_rule_id s =
  match String.split_on_char '.' s with
  | [] | [ _ ] -> false
  | first :: rest ->
      String.length first > 0
      && String.for_all is_lower_alnum first
      && List.for_all
           (fun part ->
             String.length part > 0
             && String.for_all (fun c -> is_lower_alnum c || c = '-') part)
           rest

let valid_semver s =
  match String.split_on_char '.' s with
  | [ a; b; c ] -> all_digits a && all_digits b && all_digits c
  | _ -> false

let valid_compiler s =
  starts_with ~prefix:"agentctl/" s
  && all_digits (strip_prefix ~prefix:"agentctl/" s)

let valid_exception_id s =
  starts_with ~prefix:"exc." s
  &&
  let rest = strip_prefix ~prefix:"exc." s in
  String.length rest > 0
  && String.for_all (fun c -> is_lower_alnum c || c = '-') rest

let valid_exception_scope s =
  let valid_prefix p =
    starts_with ~prefix:p s
    &&
    let rest = String.trim (strip_prefix ~prefix:p s) in
    rest <> "" && not (String.for_all (fun c -> c = '*') rest)
  in
  (* Path-only scopes need a changed-file context that render does not have;
     accepting them would claim a narrow exception the artifact cannot prove.
     Repo scopes may still embed a path qualifier (repo:name/path:...). *)
  valid_prefix "repo:" || valid_prefix "project:" || valid_prefix "task:"

let placeholders = [ "todo"; "tbd"; "unassigned"; "owner"; "placeholder" ]

let is_placeholder s =
  let t = String.lowercase_ascii (String.trim s) in
  t = "" || List.mem t placeholders

let parse_supersedes s =
  match String.rindex_opt s '@' with
  | None -> None
  | Some i ->
      let id = String.sub s 0 i in
      let rev = String.sub s (i + 1) (String.length s - i - 1) in
      if id = "" || not (all_digits rev) then None
      else (
        match int_of_string_opt rev with
        | Some n -> Some (id, n)
        | None -> None)

let is_leap y = (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0

let days_in_month y = function
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap y then 29 else 28
  | _ -> 0

let parse_date s =
  if String.length s <> 10 || s.[4] <> '-' || s.[7] <> '-' then None
  else
    let digits_at i len =
      let rec loop j =
        j >= i + len || (is_digit s.[j] && loop (j + 1))
      in
      loop i
    in
    if not (digits_at 0 4 && digits_at 5 2 && digits_at 8 2) then None
    else
      let y = int_of_string (String.sub s 0 4) in
      let m = int_of_string (String.sub s 5 2) in
      let d = int_of_string (String.sub s 8 2) in
      if y < 1 || m < 1 || m > 12 || d < 1 || d > days_in_month y m then None
      else Some (y, m, d)

let utc_today () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d" (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday

let sha256_hex s = Digestif.SHA256.(digest_string s |> to_hex)

let compare_diagnostic a b =
  match String.compare a.path b.path with
  | 0 -> (
      match (a.line, b.line) with
      | None, None -> String.compare a.code b.code
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some x, Some y ->
          let c = Int.compare x y in
          if c <> 0 then c else String.compare a.code b.code)
  | c -> c

let compare_diagnostic_full a b =
  let c = compare_diagnostic a b in
  if c <> 0 then c else String.compare a.message b.message

let normalize_diagnostics ds =
  List.sort_uniq compare_diagnostic_full ds |> List.sort compare_diagnostic

(* ------------------------------------------------------------------ *)
(* YAML decoding helpers                                              *)
(* ------------------------------------------------------------------ *)

type ctx = {
  path : string;
  line : int;
  base : string;
  ds : diagnostic list ref;
}

let mk_ctx ~path ~line ~base ~ds = { path; line; base; ds }

let ccode ctx suffix =
  if ctx.base = "" then suffix else ctx.base ^ "." ^ suffix

let warn ctx suffix message =
  ctx.ds := diag (ccode ctx suffix) ctx.path (Some ctx.line) message :: !(ctx.ds)

let warn_code ctx code message =
  ctx.ds := diag code ctx.path (Some ctx.line) message :: !(ctx.ds)

let unknown_fields ctx known = function
  | Map entries ->
      List.iter
        (fun (key, _) ->
          if not (List.mem key known) then
            warn ctx "unknown-field" (Printf.sprintf "unknown field '%s'" key))
        entries
  | _ -> ()

let shape_must_be_mapping ctx what = function
  | Map _ | Null -> ()
  | _ -> warn_code ctx "yaml.type" (Printf.sprintf "%s must be a mapping" what)

let as_string ctx t key =
  match Yaml_lite.get key t with
  | Some (String s) -> s
  | Some Null -> ""
  | Some _ ->
      warn_code ctx "yaml.type"
        (Printf.sprintf "field '%s' must be a string" key);
      ""
  | None -> ""

let as_opt_string ctx t key =
  match Yaml_lite.get key t with
  | Some (String s) -> Some s
  | Some Null | None -> None
  | Some _ ->
      warn_code ctx "yaml.type"
        (Printf.sprintf "field '%s' must be a string" key);
      None

let as_int ctx t key default =
  match Yaml_lite.get key t with
  | Some (Int i) -> i
  | Some Null | None -> default
  | Some _ ->
      warn_code ctx "yaml.type"
        (Printf.sprintf "field '%s' must be an integer" key);
      default

let as_bool ctx t key default =
  match Yaml_lite.get key t with
  | Some (Bool b) -> b
  | Some Null | None -> default
  | Some _ ->
      warn_code ctx "yaml.type"
        (Printf.sprintf "field '%s' must be a boolean" key);
      default

let as_string_list ctx t key =
  match Yaml_lite.get key t with
  | None | Some Null -> []
  | Some (List items) ->
      List.filter_map
        (fun item ->
          match item with
          | String s -> Some s
          | _ ->
              warn_code ctx "yaml.type"
                (Printf.sprintf "every item of '%s' must be a string" key);
              None)
        items
  | Some _ ->
      warn_code ctx "yaml.type"
        (Printf.sprintf "field '%s' must be a list of strings" key);
      []

let as_string_list_nonempty ctx t key =
  (match Yaml_lite.get key t with
  | Some (List []) ->
      warn ctx key (Printf.sprintf "field '%s' must not be an empty list" key)
  | _ -> ());
  as_string_list ctx t key

let as_string_pairs ctx t key =
  match Yaml_lite.get key t with
  | None | Some Null -> []
  | Some (List items) ->
      List.filter_map
        (fun item ->
          match item with
          | Map _ ->
              let sub =
                {
                  ctx with
                  base = ccode ctx key;
                  ds = ctx.ds;
                }
              in
              unknown_fields sub [ "name"; "role" ] item;
              let name = as_string sub item "name" in
              let role = as_string sub item "role" in
              Some (name, role)
          | _ ->
              warn_code ctx "yaml.type"
                (Printf.sprintf "every item of '%s' must be a mapping" key);
              None)
        items
  | Some _ ->
      warn_code ctx "yaml.type"
        (Printf.sprintf "field '%s' must be a list of mappings" key);
      []

let as_controls ctx t key =
  match Yaml_lite.get key t with
  | None | Some Null -> []
  | Some (Map entries) ->
      entries
      |> List.map (fun (cls, value) ->
             let controls =
               match value with
               | List items ->
                   List.filter_map
                     (function
                       | String s -> Some s
                       | _ ->
                           warn_code ctx "yaml.type"
                             (Printf.sprintf
                                "every control under '%s.%s' must be a string"
                                key cls);
                           None)
                     items
               | Null -> []
               | _ ->
                   warn_code ctx "yaml.type"
                     (Printf.sprintf "controls under '%s.%s' must be a list" key
                        cls);
                   []
             in
             (cls, controls))
      |> List.sort compare
  | Some _ ->
      warn_code ctx "yaml.type"
        (Printf.sprintf "field '%s' must be a mapping" key);
      []

(* ------------------------------------------------------------------ *)
(* Model decoding                                                     *)
(* ------------------------------------------------------------------ *)

let strength_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "must" -> Must
  | "should" -> Should
  | "may" -> May
  | _ -> Must

let kind_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "instruction" -> Instruction
  | "procedure" -> Procedure
  | "knowledge" -> Knowledge
  | "capability" -> Capability
  | _ -> Instruction

let class_of_string s =
  match String.lowercase_ascii (String.trim s) with
  | "advisory" -> Advisory
  | "gate" -> Gate
  | "consent" -> Consent
  | "prohibition" -> Prohibition
  | _ -> Advisory

let class_of_key = function
  | "advisory" -> Some Advisory
  | "gate" -> Some Gate
  | "consent" -> Some Consent
  | "prohibition" -> Some Prohibition
  | _ -> None

let controls_for = function
  | Advisory -> [ "instruction"; "skill" ]
  | Gate -> [ "ci-verification"; "ci-review"; "evidence-verifier" ]
  | Consent -> [ "host-hook"; "wrapper"; "permissions" ]
  | Prohibition ->
      [ "filesystem"; "network"; "tool"; "sandbox"; "permissions" ]

let known_actions =
  [
    "repo.read";
    "repo.write";
    "fs.read";
    "fs.delete";
    "net.fetch";
    "tool.output";
    "web.read";
    "memory.read";
    "memory.put";
    "vcs.push";
    "vcs.history";
    "deploy.publish";
    "shared.mutate";
    "delivery.verify";
    "delivery.review";
    "task.report";
    "*";
  ]

let decode_enforcement ctx t =
  match Yaml_lite.get "enforcement" t with
  | None | Some Null -> None
  | Some (Map _ as m) ->
      let sub = { ctx with base = ccode ctx "enforcement"; ds = ctx.ds } in
      unknown_fields sub [ "class"; "controls" ] m;
      let raw = as_string sub m "class" in
      let cls =
        match class_of_key (String.lowercase_ascii (String.trim raw)) with
        | Some c -> c
        | None ->
            warn sub "class"
              (Printf.sprintf
                 "unknown enforcement class '%s'; expected advisory, gate, \
                  consent, or prohibition"
                 raw);
            Advisory
      in
      let controls = as_string_list sub m "controls" in
      Some { cls; controls }
  | Some _ ->
      warn_code ctx "yaml.type" "field 'enforcement' must be a mapping";
      None

let decode_guards ctx t =
  match Yaml_lite.get "guards" t with
  | None | Some Null -> []
  | Some (List items) ->
      List.filter_map
        (fun item ->
          match item with
          | Map _ ->
              let sub = { ctx with base = ccode ctx "guard"; ds = ctx.ds } in
              unknown_fields sub [ "action"; "paths"; "hosts" ] item;
              let action = as_string sub item "action" in
              let paths = as_string_list_nonempty sub item "paths" in
              let hosts = as_string_list_nonempty sub item "hosts" in
              Some { action; paths; hosts }
          | _ ->
              warn_code ctx "yaml.type" "every guard must be a mapping";
              None)
        items
  | Some _ ->
      warn_code ctx "yaml.type" "field 'guards' must be a list";
      []

let decode_rule ctx t ~body ~source ~line ~layer =
  unknown_fields ctx
    [
      "id";
      "revision";
      "owner";
      "strength";
      "scope";
      "final";
      "kind";
      "title";
      "enforcement";
      "actions";
      "guards";
      "key";
      "value";
      "relaxes";
      "supersedes";
    ]
    t;
  let id = as_string ctx t "id" in
  let revision = as_int ctx t "revision" 0 in
  let owner = as_string ctx t "owner" in
  let raw_strength = as_string ctx t "strength" in
  let strength =
    match String.lowercase_ascii (String.trim raw_strength) with
    | "must" -> Must
    | "should" -> Should
    | "may" -> May
    | _ ->
        warn ctx "strength"
          (Printf.sprintf
             "unknown strength '%s'; expected must, should, or may"
             raw_strength);
        Must
  in
  let scope = as_string_list ctx t "scope" in
  let final =
    match Yaml_lite.get "final" t with
    | Some (Bool b) -> b
    | Some Null | None ->
        warn ctx "final" "missing required boolean field 'final'";
        false
    | Some _ ->
        warn_code ctx "yaml.type" "field 'final' must be a boolean";
        false
  in
  let raw_kind = as_string ctx t "kind" in
  let kind =
    match String.lowercase_ascii (String.trim raw_kind) with
    | "instruction" -> Instruction
    | "procedure" -> Procedure
    | "knowledge" -> Knowledge
    | "capability" -> Capability
    | _ ->
        warn ctx "kind"
          (Printf.sprintf
             "unknown kind '%s'; expected instruction, procedure, knowledge, \
              or capability"
             raw_kind);
        Instruction
  in
  let title = as_opt_string ctx t "title" in
  let enforcement = decode_enforcement ctx t in
  let actions = as_string_list ctx t "actions" in
  let guards = decode_guards ctx t in
  let key = as_opt_string ctx t "key" in
  let value = as_opt_string ctx t "value" in
  let relaxes = as_string_list ctx t "relaxes" in
  let supersedes = as_opt_string ctx t "supersedes" in
  {
    id;
    revision;
    owner;
    strength;
    scope;
    final;
    kind;
    title;
    enforcement;
    actions;
    guards;
    key;
    value;
    relaxes;
    supersedes;
    body;
    source;
    line;
    layer;
  }

let decode_policy ~path ds tree =
  let ctx = mk_ctx ~path ~line:1 ~base:"policy" ~ds in
  shape_must_be_mapping ctx "policy.yaml" tree;
  unknown_fields ctx
    [ "schema"; "release"; "compiler"; "description"; "maintainers"; "budget" ]
    tree;
  let schema = as_string ctx tree "schema" in
  let release = as_string ctx tree "release" in
  let compiler = as_string ctx tree "compiler" in
  let description = as_opt_string ctx tree "description" in
  let maintainers = as_string_pairs ctx tree "maintainers" in
  let budget_ctx = { ctx with base = "policy.budget"; ds } in
  let core_max_bytes, max_rules =
    match Yaml_lite.get "budget" tree with
    | Some (Map _ as b) ->
        unknown_fields budget_ctx [ "core_max_bytes"; "max_rules" ] b;
        (as_int budget_ctx b "core_max_bytes" 0, as_int budget_ctx b "max_rules" 0)
    | Some Null | None -> (0, 0)
    | Some _ ->
        warn_code ctx "yaml.type" "field 'budget' must be a mapping";
        (0, 0)
  in
  {
    schema;
    release;
    compiler;
    description;
    maintainers;
    core_max_bytes;
    max_rules;
    source = path;
  }

let decode_role ~path ds tree =
  let ctx = mk_ctx ~path ~line:1 ~base:"role" ~ds in
  shape_must_be_mapping ctx "role" tree;
  unknown_fields ctx
    [
      "name";
      "description";
      "charter";
      "output_contract";
      "memory_contract";
      "relaxes";
      "additions";
    ]
    tree;
  {
    name = as_string ctx tree "name";
    description = as_string ctx tree "description";
    charter = as_string ctx tree "charter";
    output_contract = as_string ctx tree "output_contract";
    memory_contract = as_string ctx tree "memory_contract";
    relaxes = as_string_list ctx tree "relaxes";
    additions = as_string_list ctx tree "additions";
    source = path;
  }

let decode_adapter ~path ds tree =
  let ctx = mk_ctx ~path ~line:1 ~base:"adapter" ~ds in
  shape_must_be_mapping ctx "adapter" tree;
  unknown_fields ctx
    [
      "id";
      "version";
      "description";
      "instructions";
      "skills";
      "roles";
      "mcp";
      "hooks";
      "permissions";
      "atomic_output";
      "controls";
      "unsupported";
    ]
    tree;
  let instructions =
    match Yaml_lite.get "instructions" tree with
    | Some (Map _ as m) ->
        let sub = { ctx with base = "adapter.instructions"; ds } in
        unknown_fields sub [ "file"; "discovery"; "includes" ] m;
        {
          file = as_string sub m "file";
          discovery = as_string_list sub m "discovery";
          includes = as_bool sub m "includes" false;
        }
    | Some Null | None -> { file = ""; discovery = []; includes = false }
    | Some _ ->
        warn_code ctx "yaml.type" "field 'instructions' must be a mapping";
        { file = ""; discovery = []; includes = false }
  in
  let skills =
    match Yaml_lite.get "skills" tree with
    | Some (Map _ as m) ->
        let sub = { ctx with base = "adapter.skills"; ds } in
        unknown_fields sub [ "dir"; "auto_discovery"; "explicit_reference" ] m;
        {
          dir = as_string sub m "dir";
          auto_discovery = as_bool sub m "auto_discovery" false;
          explicit_reference = as_bool sub m "explicit_reference" false;
        }
    | Some Null | None ->
        { dir = ""; auto_discovery = false; explicit_reference = false }
    | Some _ ->
        warn_code ctx "yaml.type" "field 'skills' must be a mapping";
        { dir = ""; auto_discovery = false; explicit_reference = false }
  in
  {
    id = as_string ctx tree "id";
    version = as_int ctx tree "version" 0;
    description = as_string ctx tree "description";
    instructions;
    skills;
    roles = as_bool ctx tree "roles" false;
    mcp = as_bool ctx tree "mcp" false;
    hooks = as_bool ctx tree "hooks" false;
    permissions = as_bool ctx tree "permissions" false;
    atomic_output = as_bool ctx tree "atomic_output" false;
    controls = as_controls ctx tree "controls";
    unsupported = as_string_list ctx tree "unsupported";
    source = path;
  }

let decode_exception ~path ~line ds tree =
  let ctx = mk_ctx ~path ~line ~base:"exception" ~ds in
  unknown_fields ctx
    [ "id"; "rule"; "scope"; "reason"; "approver"; "expires" ]
    tree;
  {
    id = as_string ctx tree "id";
    rule = as_string ctx tree "rule";
    scope = as_string ctx tree "scope";
    reason = as_string ctx tree "reason";
    approver = as_string ctx tree "approver";
    expires = as_string ctx tree "expires";
    source = path;
    line;
  }

let decode_skill ~path ~body ~files ds tree =
  let ctx = mk_ctx ~path ~line:1 ~base:"skill" ~ds in
  shape_must_be_mapping ctx "skill front matter" tree;
  unknown_fields ctx
    [
      "name";
      "description";
      "triggers";
      "prerequisites";
      "stop_conditions";
      "verification";
      "rules";
    ]
    tree;
  {
    name = as_string ctx tree "name";
    description = as_string ctx tree "description";
    triggers = as_string_list ctx tree "triggers";
    prerequisites = as_string_list ctx tree "prerequisites";
    stop_conditions = as_string_list ctx tree "stop_conditions";
    verification = as_string_list ctx tree "verification";
    rules = as_string_list ctx tree "rules";
    body;
    files;
    source = path;
  }

(* ------------------------------------------------------------------ *)
(* Front matter and fragments                                         *)
(* ------------------------------------------------------------------ *)

let split_lines text =
  String.split_on_char '\n' text
  |> List.map (fun line ->
         if ends_with ~suffix:"\r" line then
           String.sub line 0 (String.length line - 1)
         else line)

let rec take n list =
  if n <= 0 then []
  else match list with [] -> [] | x :: xs -> x :: take (n - 1) xs

let rec drop n list =
  if n <= 0 then list
  else match list with [] -> [] | _ :: xs -> drop (n - 1) xs

let yaml_diagnostic path message =
  let prefix = path ^ ":" in
  let rest = strip_prefix ~prefix message in
  match String.index_opt rest ':' with
  | Some i ->
      let line_part = String.sub rest 0 i in
      let msg =
        String.sub rest (i + 1) (String.length rest - i - 1) |> String.trim
      in
      let line =
        match int_of_string_opt (String.trim line_part) with
        | Some n -> Some n
        | None -> None
      in
      diag "yaml.parse" path line msg
  | None -> diag "yaml.parse" path None message

let parse_frontmatter ~path text =
  let lines = split_lines text in
  match lines with
  | first :: rest when String.trim first = "---" -> (
      let rec find_close i = function
        | [] -> None
        | line :: tl -> if String.trim line = "---" then Some i else find_close (i + 1) tl
      in
      match find_close 0 rest with
      | None ->
          Error
            [
              diag "frontmatter.unterminated" path (Some 1)
                "front matter opened with '---' but never closed";
            ]
      | Some i -> (
          let front = String.concat "\n" (take i rest) in
          let body = String.concat "\n" (drop (i + 1) rest) in
          let body_offset = i + 3 in
          match Yaml_lite.parse_string ~source:path front with
          | Error message -> Error [ yaml_diagnostic path message ]
          | Ok Null -> Ok (Null, body, body_offset)
          | Ok (Map _ as tree) -> Ok (tree, body, body_offset)
          | Ok _ ->
              Error
                [
                  diag "frontmatter.shape" path (Some 1)
                    "front matter must be a mapping";
                ]))
  | _ -> Ok (Null, text, 1)

let read_abs path =
  try Ok (In_channel.with_open_bin path In_channel.input_all)
  with Sys_error _ -> Error ()

let read_rel root rel = read_abs (Filename.concat root rel)

let decode_fragment ~path ds tree ~body =
  let ctx = mk_ctx ~path ~line:1 ~base:"fragment" ~ds in
  match tree with
  | Null -> []
  | Map _ ->
      unknown_fields ctx [ "rules"; "relaxes" ] tree;
      let top_relaxes = as_string_list ctx tree "relaxes" in
      let items =
        match Yaml_lite.get "rules" tree with
        | None | Some Null -> []
        | Some (List items) -> items
        | Some _ ->
            warn_code ctx "yaml.type" "field 'rules' must be a list";
            []
      in
      let rules =
        List.filter_map
          (fun item ->
            match item with
            | Map _ ->
                let line = 1 in
                let rule_ctx = mk_ctx ~path ~line ~base:"rule" ~ds in
                let rule =
                  decode_rule rule_ctx item ~body ~source:path ~line
                    ~layer:Repository
                in
                let relaxes = List.sort_uniq String.compare (rule.relaxes @ top_relaxes) in
                Some { rule with relaxes }
            | _ ->
                warn_code ctx "yaml.type" "every item of 'rules' must be a mapping";
                None)
          items
      in
      if top_relaxes <> [] && rules = [] then
        warn ctx "relaxes-without-rules"
          "top-level 'relaxes' has no inline rule to attach to";
      rules
  | _ ->
      warn_code ctx "frontmatter.shape" "front matter must be a mapping";
      []

let load_fragment ~path =
  match read_abs path with
  | Error () ->
      Error [ diag "source.read" path None "cannot read fragment file" ]
  | Ok text -> (
      match parse_frontmatter ~path text with
      | Error ds -> Error ds
      | Ok (tree, body, _body_offset) ->
          let ds = ref [] in
          let rules = decode_fragment ~path ds tree ~body in
          match normalize_diagnostics !ds with
          | [] -> Ok (rules, body, [ (path, sha256_hex text) ])
          | ds -> Error ds)

let load_fragment_rel ~root ~rel =
  if not (Filename.is_relative rel) then
    Error
      [ diag "source.read" rel None
          "fragment path must be relative to the repository root" ]
  else
    match read_rel root rel with
    | Error () ->
        Error [ diag "source.read" rel None "cannot read fragment file" ]
    | Ok text -> (
        match parse_frontmatter ~path:rel text with
        | Error ds -> Error ds
        | Ok (tree, body, _body_offset) ->
            let ds = ref [] in
            let rules = decode_fragment ~path:rel ds tree ~body in
            match normalize_diagnostics !ds with
            | [] -> Ok (rules, body, [ (rel, sha256_hex text) ])
            | ds -> Error ds)

(* ------------------------------------------------------------------ *)
(* Directory walking and loading                                      *)
(* ------------------------------------------------------------------ *)

let dir_exists root rel =
  try Sys.is_directory (Filename.concat root rel) with Sys_error _ -> false

let join rel name = if rel = "" then name else rel ^ "/" ^ name

let list_entries root rel =
  if not (dir_exists root rel) then []
  else
    try
      Sys.readdir (Filename.concat root rel) |> Array.to_list
      |> List.sort String.compare
    with Sys_error _ -> []

(* Follow source files through Dune's sandbox symlinks, but never recurse into
   the same directory twice. A symlink loop therefore terminates. *)
let rec walk_files ?(seen = []) root rel =
  let absolute = Filename.concat root rel in
  match Unix.stat absolute with
  | exception Unix.Unix_error _ | exception Sys_error _ -> []
  | stats when stats.Unix.st_kind = Unix.S_DIR ->
      let key = (stats.Unix.st_dev, stats.Unix.st_ino) in
      if List.mem key seen then []
      else
        list_entries root rel
        |> List.concat_map (fun name ->
               if name = "" || name.[0] = '.' then []
               else walk_files ~seen:(key :: seen) root (join rel name))
  | stats when stats.Unix.st_kind = Unix.S_REG -> [ rel ]
  | _ -> []

let load_rules root (errors : diagnostic list ref) (files : (string * string) list ref) =
  let rels =
    list_entries root "rules"
    |> List.filter (fun name ->
           name <> "" && name.[0] <> '.' && Filename.check_suffix name ".md")
  in
  let rules = ref [] in
  List.iter
    (fun name ->
      let rel = "rules/" ^ name in
      match read_rel root rel with
      | Error () ->
          errors :=
            diag "source.read" rel None "cannot read rule file" :: !errors
      | Ok text ->
          files := (rel, sha256_hex text) :: !files;
          (match parse_frontmatter ~path:rel text with
          | Error ds -> errors := List.rev_append ds !errors
          | Ok (Null, _, _) ->
              errors :=
                diag "rule.missing" rel (Some 1)
                  "rule file has no front matter; expected one rule per file"
                :: !errors
          | Ok (Map _ as tree, body, _) ->
              let ctx = mk_ctx ~path:rel ~line:1 ~base:"rule" ~ds:errors in
              let rule =
                decode_rule ctx tree ~body ~source:rel ~line:1
                  ~layer:Organization
              in
              rules := rule :: !rules
          | Ok _ -> ()))
    rels;
  List.rev !rules

let load_roles root (errors : diagnostic list ref) (files : (string * string) list ref) =
  list_entries root "roles"
  |> List.filter (fun name ->
         name <> "" && name.[0] <> '.' && Filename.check_suffix name ".yaml")
  |> List.filter_map (fun name ->
         let rel = "roles/" ^ name in
         match read_rel root rel with
         | Error () ->
             errors :=
               diag "source.read" rel None "cannot read role file" :: !errors;
             None
         | Ok text ->
             files := (rel, sha256_hex text) :: !files;
             (match Yaml_lite.parse_string ~source:rel text with
             | Error message ->
                 errors := yaml_diagnostic rel message :: !errors;
                 None
             | Ok tree -> Some (decode_role ~path:rel errors tree)))

let load_adapters root (errors : diagnostic list ref)
    (files : (string * string) list ref) =
  list_entries root "adapters"
  |> List.filter (fun name ->
         name <> "" && name.[0] <> '.' && Filename.check_suffix name ".yaml")
  |> List.filter_map (fun name ->
         let rel = "adapters/" ^ name in
         match read_rel root rel with
         | Error () ->
             errors :=
               diag "source.read" rel None "cannot read adapter file" :: !errors;
             None
         | Ok text ->
             files := (rel, sha256_hex text) :: !files;
             (match Yaml_lite.parse_string ~source:rel text with
             | Error message ->
                 errors := yaml_diagnostic rel message :: !errors;
                 None
             | Ok tree -> Some (decode_adapter ~path:rel errors tree)))

let load_exceptions root (errors : diagnostic list ref)
    (files : (string * string) list ref) =
  let rel = "exceptions.yaml" in
  if not (Sys.file_exists (Filename.concat root rel)) then []
  else
    match read_rel root rel with
    | Error () ->
        errors :=
          diag "source.read" rel None "cannot read exceptions file" :: !errors;
        []
    | Ok text -> (
        files := (rel, sha256_hex text) :: !files;
        match Yaml_lite.parse_string ~source:rel text with
        | Error message ->
            errors := yaml_diagnostic rel message :: !errors;
            []
        | Ok Null -> []
        | Ok (List items) ->
            items
            |> List.mapi (fun i item ->
                   match item with
                   | Map _ ->
                       let line = i + 1 in
                       Some (decode_exception ~path:rel ~line errors item)
                   | _ ->
                       errors :=
                         diag "yaml.type" rel (Some (i + 1))
                           "every exception must be a mapping"
                         :: !errors;
                       None)
            |> List.filter_map Fun.id
        | Ok _ ->
            errors :=
              diag "yaml.type" rel (Some 1)
                "exceptions.yaml must contain a top-level list"
              :: !errors;
            [])

(* Reviewed fragments participate in the release digest as source bytes, but
   the loader deliberately does not parse them. [render_erde] reads and hash-
   checks the one fragment it consumes. *)
let load_fragments root (errors : diagnostic list ref)
    (files : (string * string) list ref) =
  list_entries root "fragments"
  |> List.filter (fun name ->
         name <> "" && name.[0] <> '.' && Filename.check_suffix name ".md"
         && not (dir_exists root ("fragments/" ^ name)))
  |> List.iter (fun name ->
         let rel = "fragments/" ^ name in
         match read_rel root rel with
         | Error () ->
             errors :=
               diag "source.read" rel None "cannot read fragment file" :: !errors
         | Ok text -> files := (rel, sha256_hex text) :: !files)

let load_skills root (errors : diagnostic list ref) (files : (string * string) list ref) =
  let names =
    list_entries root "skills"
    |> List.filter (fun name ->
           name <> "" && name.[0] <> '.' && dir_exists root ("skills/" ^ name))
  in
  List.filter_map
    (fun name ->
      let dir = "skills/" ^ name in
      let all = walk_files root dir in
      let read_files =
        List.map (fun rel -> (rel, read_rel root rel)) all
      in
      List.iter
        (fun (rel, contents) ->
          match contents with
          | Ok text -> files := (rel, sha256_hex text) :: !files
          | Error () ->
              errors :=
                diag "source.read" rel None "cannot read skill file" :: !errors)
        read_files;
      let skill_md = dir ^ "/SKILL.md" in
      match List.assoc_opt skill_md read_files with
      | None ->
          errors :=
            diag "skill.missing" skill_md None
              "skill directory has no SKILL.md" :: !errors;
          None
      | Some (Error ()) -> None
      | Some (Ok text) -> (
          match parse_frontmatter ~path:skill_md text with
          | Error ds ->
              errors := List.rev_append ds !errors;
              None
          | Ok (tree, body, _) ->
              let relative_files =
                read_files
                |> List.map (fun (rel, contents) ->
                       let rel_inside = strip_prefix ~prefix:(dir ^ "/") rel in
                       let hash =
                         match contents with
                         | Ok text -> sha256_hex text
                         | Error () -> ""
                       in
                       (rel_inside, hash))
                |> List.sort compare
              in
              Some
                (decode_skill ~path:skill_md ~body ~files:relative_files errors
                   tree)))
    names

let empty_policy ~source =
  {
    schema = "";
    release = "";
    compiler = "";
    description = None;
    maintainers = [];
    core_max_bytes = 0;
    max_rules = 0;
    source;
  }

let load ~root =
  let errors = ref [] in
  let files = ref [] in
  let policy =
    let rel = "policy.yaml" in
    match read_rel root rel with
    | Error () ->
        errors :=
          diag "source.missing" rel None
            "policy.yaml is required at the source root" :: !errors;
        empty_policy ~source:rel
    | Ok text ->
        files := (rel, sha256_hex text) :: !files;
        (match Yaml_lite.parse_string ~source:rel text with
        | Error message ->
            errors := yaml_diagnostic rel message :: !errors;
            empty_policy ~source:rel
        | Ok tree -> decode_policy ~path:rel errors tree)
  in
  let rules = load_rules root errors files in
  let roles = load_roles root errors files in
  let adapters = load_adapters root errors files in
  let skills = load_skills root errors files in
  let () = load_fragments root errors files in
  let exceptions = load_exceptions root errors files in
  match normalize_diagnostics !errors with
  | [] ->
      Ok
        {
          root;
          policy;
          rules;
          skills;
          roles;
          adapters;
          exceptions;
          files = List.sort_uniq compare !files;
        }
  | ds -> Error ds

(* ------------------------------------------------------------------ *)
(* Secret and path scanning                                           *)
(* ------------------------------------------------------------------ *)

let has_private_absolute_path text =
  List.exists
    (fun prefix -> contains_sub text prefix)
    [ "/home/"; "/Users/"; "/root/"; "/private/"; "/tmp/"; "/var/folders/" ]
  || contains_sub text "\\Users\\"

let is_placeholder_secret value =
  let v = String.lowercase_ascii value in
  v = ""
  || starts_with ~prefix:"$" v
  || starts_with ~prefix:"<" v
  || starts_with ~prefix:"{" v
  || List.mem v
       [
         "redacted";
         "placeholder";
         "example";
         "changeme";
         "change_me";
         "change-me";
         "dummy";
         "none";
         "secret";
         "xxx";
         "xxxx";
         "your-token-here";
         "your-password-here";
         "replace-me";
         "required";
       ]
  || String.for_all (fun c -> c = '*' || c = 'x' || c = 'X' || c = '-') value

let strip_value_noise s =
  let v = String.trim s in
  let n = String.length v in
  let v =
    if n >= 2 then
      let first = v.[0] and last = v.[n - 1] in
      if
        (first = '"' && last = '"')
        || (first = '\'' && last = '\'')
        || (first = '`' && last = '`')
      then String.sub v 1 (n - 2)
      else v
    else v
  in
  let rec strip_trailing v =
    let n = String.length v in
    if n = 0 then v
    else
      match v.[n - 1] with
      | '.' | ',' | ';' | ':' | ')' | ']' | '}' ->
          strip_trailing (String.sub v 0 (n - 1))
      | _ -> String.trim v
  in
  strip_trailing (String.trim v)

let find_credential_assignment line =
  let n = String.length line in
  let rec scan i =
    if i >= n then None
    else
      let keyword_len =
        if starts_with ~prefix:"password" (String.sub line i (n - i)) then Some 8
        else if starts_with ~prefix:"token" (String.sub line i (n - i)) then Some 5
        else None
      in
      match keyword_len with
      | None -> scan (i + 1)
      | Some len ->
          let boundary = i = 0 || not (is_word_char line.[i - 1]) in
          if not boundary then scan (i + 1)
          else
            let j = ref (i + len) in
            while !j < n && (line.[!j] = ' ' || line.[!j] = '\t') do
              incr j
            done;
            if !j >= n || line.[!j] <> '=' then scan (i + 1)
            else (
              incr j;
              while !j < n && (line.[!j] = ' ' || line.[!j] = '\t') do
                incr j
              done;
              if !j >= n then None
              else
                let start = !j in
                let first = line.[start] in
                if first = '"' || first = '\'' || first = '`' then begin
                  incr j;
                  while !j < n && line.[!j] <> first do
                    incr j
                  done;
                  if !j < n then incr j;
                  let value =
                    strip_value_noise
                      (String.sub line start (!j - start))
                  in
                  if value = "" || is_placeholder_secret value then None
                  else Some (String.sub line i len)
                end
                else begin
                  while
                    !j < n && line.[!j] <> ' ' && line.[!j] <> '\t'
                    && line.[!j] <> ';' && line.[!j] <> ','
                  do
                    incr j
                  done;
                  let value =
                    strip_value_noise
                      (String.sub line start (!j - start))
                  in
                  if value = "" || is_placeholder_secret value then None
                  else Some (String.sub line i len)
                end)
  in
  scan 0

let secret_findings text =
  let findings = ref [] in
  split_lines text
  |> List.iter (fun line ->
         if contains_sub line "-----BEGIN" && contains_sub line "PRIVATE KEY-----"
         then findings := "secret.private-key" :: !findings;
         let n = String.length line in
         for i = 0 to n - 20 do
           if String.sub line i 4 = "AKIA"
              && String.for_all is_upper_alnum (String.sub line (i + 4) 16)
           then findings := "secret.aws-key" :: !findings
         done;
         match find_credential_assignment line with
         | Some _ -> findings := "secret.credential-assignment" :: !findings
         | None -> ());
  List.sort_uniq String.compare !findings

(* ------------------------------------------------------------------ *)
(* Validation                                                         *)
(* ------------------------------------------------------------------ *)

let layer_name = function
  | Organization -> "organization"
  | Repository -> "repository"
  | Role -> "role"

let validate ?today src =
  let today_value =
    match today with
    | Some d -> String.trim d
    | None -> (
        match Sys.getenv_opt "NOMIA_TODAY" with
        | Some d when String.trim d <> "" -> String.trim d
        | _ -> utc_today ())
  in
  let ds = ref [] in
  let add code path line message = ds := diag code path line message :: !ds in
  let org_rules = List.filter (fun (r : rule) -> r.layer = Organization) src.rules in
  let find_org id = List.find_opt (fun (r : rule) -> r.id = id) org_rules in
  let find_any id = List.find_opt (fun (r : rule) -> r.id = id) src.rules in

  (* Policy *)
  let p = src.policy in
  if p.schema <> "nomia/v1" then
    add "policy.schema" p.source None
      (Printf.sprintf
         "policy schema must be 'nomia/v1', found '%s'" p.schema);
  if not (valid_semver p.release) then
    add "policy.release" p.source None
      (Printf.sprintf
         "release must be a numeric MAJOR.MINOR.PATCH version, found '%s'"
         p.release);
  if not (valid_compiler p.compiler) then
    add "policy.compiler" p.source None
      (Printf.sprintf "compiler must be 'agentctl/N', found '%s'" p.compiler);
  if p.maintainers = [] then
    add "policy.maintainers" p.source None
      "maintainers must name at least one release owner";
  List.iter
    (fun (name, role) ->
      if String.trim name = "" then
        add "policy.maintainers" p.source None
          "every maintainer must have a non-empty name";
      if
        not
          (List.mem role
             [ "engineering-owner"; "security-owner"; "policy-owner" ])
      then
        add "policy.maintainers" p.source None
          (Printf.sprintf
             "maintainer '%s' has unknown role '%s'; expected engineering-owner, \
              security-owner, or policy-owner"
             name role))
    p.maintainers;
  if p.core_max_bytes <= 0 || p.max_rules <= 0 then
    add "policy.budget" p.source None
      "budget.core_max_bytes and budget.max_rules must both be positive";

  (* Rules *)
  let seen_ids = Hashtbl.create 16 in
  let key_claims = Hashtbl.create 16 in
  List.iter
    (fun (r : rule) ->
      let path = r.source and line = Some r.line in
      if not (valid_rule_id r.id) then
        add "rule.id.invalid" path line
          (Printf.sprintf
             "rule id '%s' must match ^[a-z0-9]+(\\.[a-z0-9-]+)+$" r.id);
      if r.revision < 1 then
        add "rule.revision" path line
          (Printf.sprintf "rule '%s' revision must be >= 1, found %d" r.id
             r.revision);
      if String.trim r.owner = "" then
        add "rule.owner.missing" path line
          (Printf.sprintf
             "rule '%s' has no owner; name a concrete release owner" r.id)
      else if is_placeholder r.owner then
        add "rule.owner.placeholder" path line
          (Printf.sprintf
             "rule '%s' owner '%s' is a placeholder; name a concrete release \
              owner"
             r.id r.owner);
      if r.scope = [] then
        add "rule.scope" path line
          (Printf.sprintf "rule '%s' must declare a non-empty scope" r.id)
      else
        List.iter
          (fun s ->
            if String.trim s = "" then
              add "rule.scope" path line
                (Printf.sprintf "rule '%s' has an empty scope entry" r.id))
          r.scope;
      if
        Hashtbl.mem seen_ids r.id
        && String.trim r.id <> ""
      then
        add "rule.duplicate-id" path line
          (Printf.sprintf "duplicate rule id '%s'" r.id)
      else if String.trim r.id <> "" then Hashtbl.replace seen_ids r.id ();
      if r.final && String.trim r.owner = "" then
        add "rule.final-owner" path line
          (Printf.sprintf
             "final rule '%s' must name a concrete owner" r.id);
      (match r.enforcement with
      | None -> ()
      | Some e ->
          let allowed = controls_for e.cls in
          List.iter
            (fun control ->
              if not (List.mem control allowed) then
                add "rule.enforcement.control" path line
                  (Printf.sprintf
                     "control '%s' is not valid for enforcement class '%s' in \
                      rule '%s'; expected one of: %s"
                     control
                     (match e.cls with
                     | Advisory -> "advisory"
                     | Gate -> "gate"
                     | Consent -> "consent"
                     | Prohibition -> "prohibition")
                     r.id (String.concat ", " allowed)))
            e.controls;
          if
            (e.cls = Consent || e.cls = Prohibition)
            && e.controls = []
          then
            add "rule.enforcement.controls" path line
              (Printf.sprintf
                 "%s rule '%s' needs at least one control; an empty control \
                  list would present an unenforceable rule as supported"
                 (match e.cls with
                 | Consent -> "consent"
                 | Prohibition -> "prohibition"
                 | _ -> "gated")
                 r.id));
      List.iter
        (fun action ->
          if not (List.mem action known_actions) then
            add "rule.action.unknown" path line
              (Printf.sprintf "rule '%s' names unknown action '%s'" r.id action))
        r.actions;
      List.iter
        (fun g ->
          if not (List.mem g.action known_actions) then
            add "rule.guard.action" path line
              (Printf.sprintf "rule '%s' guard names unknown action '%s'" r.id
                 g.action);
          List.iter
            (fun p ->
              if String.trim p = "" then
                add "rule.guard.paths" path line
                  (Printf.sprintf "rule '%s' guard has an empty path" r.id)
              else if has_private_absolute_path p then
                add "rule.guard.private-path" path line
                  (Printf.sprintf
                     "rule '%s' guard path '%s' is a private absolute machine \
                      path; use a repository-relative glob"
                     r.id p))
            g.paths;
          List.iter
            (fun h ->
              if String.trim h = "" then
                add "rule.guard.hosts" path line
                  (Printf.sprintf "rule '%s' guard has an empty host" r.id))
            g.hosts)
        r.guards;
      (match (r.key, r.value) with
      | Some _, Some _ | None, None -> ()
      | _ ->
          add "rule.key-value" path line
            (Printf.sprintf
               "rule '%s' must provide both 'key' and 'value', or neither" r.id));
      if r.layer = Organization && r.relaxes <> [] then
        add "rule.relaxes.org" path line
          (Printf.sprintf
             "organization rule '%s' may not relax another rule; only lower \
              layers may relax"
             r.id)
      else
        List.iter
          (fun target ->
            match find_org target with
            | None ->
                add "rule.relaxes.target" path line
                  (Printf.sprintf
                     "rule '%s' relaxes unknown organization rule '%s'" r.id
                     target)
            | Some t when t.final ->
                add "rule.relaxes.final" path line
                  (Printf.sprintf
                     "rule '%s' relaxes final organization rule '%s' \
                      (unauthorized override)"
                     r.id target)
            | Some _ -> ())
          r.relaxes;
      (match r.supersedes with
      | None -> ()
      | Some s -> (
          match parse_supersedes s with
          | None ->
              add "rule.supersedes" path line
                (Printf.sprintf
                   "rule '%s' supersedes value '%s' must be 'id@revision'" r.id s)
          | Some (sid, rev) ->
              if sid = r.id then (
                if rev >= r.revision then
                  add "rule.supersedes" path line
                    (Printf.sprintf
                       "rule '%s' supersedes its own revision %d, but must name \
                        a lower revision than %d"
                       r.id rev r.revision))
              else
                match find_any sid with
                | None ->
                    add "rule.supersedes" path line
                      (Printf.sprintf
                         "rule '%s' supersedes unknown rule '%s'" r.id sid)
                | Some t when t.revision > rev ->
                    add "rule.supersedes" path line
                      (Printf.sprintf
                         "rule '%s' supersedes '%s@%d', but that rule is at \
                          revision %d"
                         r.id sid rev t.revision)
                | Some _ -> ()));
      (match r.key with
      | None -> ()
      | Some key ->
          let slot = (layer_name r.layer, key) in
          (match Hashtbl.find_opt key_claims slot with
          | Some (other_id, other_value) ->
              if other_value <> r.value then
                add "rule.key-conflict" path line
                  (Printf.sprintf
                     "rule '%s' claims key '%s' with a different value than rule \
                      '%s' at the same layer"
                     r.id key other_id)
          | None -> Hashtbl.replace key_claims slot (r.id, r.value))))
    src.rules;

  (* Roles *)
  let seen_role_names = Hashtbl.create 8 in
  List.iter
    (fun (role : role) ->
      let path = role.source in
      if not (is_kebab_case role.name) then
        add "role.name" path None
          (Printf.sprintf
             "role name '%s' must be kebab-case (lowercase letters, digits, and \
              single hyphens)"
             role.name);
      if Hashtbl.mem seen_role_names role.name && role.name <> "" then
        add "role.duplicate" path None
          (Printf.sprintf "duplicate role name '%s'" role.name)
      else if role.name <> "" then Hashtbl.replace seen_role_names role.name ();
      if String.trim role.description = "" then
        add "role.description" path None
          (Printf.sprintf "role '%s' needs a description" role.name);
      if String.trim role.charter = "" then
        add "role.charter" path None
          (Printf.sprintf "role '%s' needs a charter" role.name);
      if String.trim role.output_contract = "" then
        add "role.output-contract" path None
          (Printf.sprintf "role '%s' needs an output_contract" role.name);
      if role.memory_contract <> "shared" then
        add "role.memory-contract" path None
          (Printf.sprintf
             "role '%s' memory_contract must be 'shared', found '%s'" role.name
             role.memory_contract);
      List.iter
        (fun target ->
          match find_org target with
          | None ->
              add "role.relaxes.target" path None
                (Printf.sprintf "role '%s' relaxes unknown rule '%s'" role.name
                   target)
          | Some t when t.final ->
              add "role.relaxes.final" path None
                (Printf.sprintf
                   "role '%s' relaxes final rule '%s' (unauthorized override)"
                   role.name target)
          | Some _ -> ())
        role.relaxes;
      List.iter
        (fun target ->
          if find_any target = None then
            add "role.additions.target" path None
              (Printf.sprintf "role '%s' adds unknown rule '%s'" role.name target))
        role.additions)
    src.roles;

  (* Adapters *)
  let known_adapter_ids = [ "generic-agents"; "opencode"; "erde"; "saguaro" ] in
  let seen_adapter_ids = Hashtbl.create 4 in
  List.iter
    (fun (adapter : adapter) ->
      let path = adapter.source in
      if not (List.mem adapter.id known_adapter_ids) then
        add "adapter.id" path None
          (Printf.sprintf
             "adapter id '%s' must be one of: %s" adapter.id
             (String.concat ", " known_adapter_ids))
      else if Hashtbl.mem seen_adapter_ids adapter.id then
        add "adapter.duplicate-id" path None
          (Printf.sprintf "duplicate adapter id '%s'" adapter.id)
      else Hashtbl.replace seen_adapter_ids adapter.id ();
      if adapter.version < 1 then
        add "adapter.version" path None
          (Printf.sprintf "adapter '%s' version must be >= 1" adapter.id);
      List.iter
        (fun (cls, controls) ->
          match class_of_key cls with
          | None ->
              add "adapter.control" path None
                (Printf.sprintf
                   "adapter '%s' has unknown control class '%s'" adapter.id cls)
          | Some c ->
              let allowed = controls_for c in
              List.iter
                (fun control ->
                  if not (List.mem control allowed) then
                    add "adapter.control" path None
                      (Printf.sprintf
                         "adapter '%s' control '%s' is not valid for class '%s'; \
                          expected one of: %s"
                         adapter.id control cls (String.concat ", " allowed)))
                controls)
        adapter.controls;
      if adapter.unsupported = [] then
        add "adapter.unsupported" path None
          (Printf.sprintf
             "adapter '%s' must list at least one unsupported capability or say \
              'none'"
             adapter.id))
    src.adapters;

  (* Exceptions *)
  let seen_exception_ids = Hashtbl.create 8 in
  List.iter
    (fun (e : exception_) ->
      let path = e.source and line = Some e.line in
      if not (valid_exception_id e.id) then
        add "exception.id" path line
          (Printf.sprintf
             "exception id '%s' must match ^exc\\.[a-z0-9-]+$" e.id)
      else if Hashtbl.mem seen_exception_ids e.id then
        add "exception.duplicate-id" path line
          (Printf.sprintf "duplicate exception id '%s'" e.id)
      else Hashtbl.replace seen_exception_ids e.id ();
      if find_any e.rule = None then
        add "exception.rule" path line
          (Printf.sprintf "exception '%s' names unknown rule '%s'" e.id e.rule);
      if not (valid_exception_scope e.scope) then
        add "exception.scope" path line
          (Printf.sprintf
             "exception '%s' scope '%s' must start with repo:, project:, or \
              task: and name a specific target (not a bare wildcard; \
              path-only scopes are not supported in this slice)"
             e.id e.scope)
      else if has_private_absolute_path e.scope then
        add "exception.private-path" path line
          (Printf.sprintf
             "exception '%s' scope names a private absolute machine path"
             e.id);
      if String.trim e.reason = "" then
        add "exception.reason" path line
          (Printf.sprintf "exception '%s' needs a reason" e.id);
      if is_placeholder e.approver then
        add "exception.approver" path line
          (Printf.sprintf
             "exception '%s' approver '%s' is a placeholder; name a concrete \
              approver"
             e.id e.approver);
      match parse_date e.expires with
      | None ->
          add "exception.expires" path line
            (Printf.sprintf
               "exception '%s' expiry '%s' must be a real calendar date in \
                YYYY-MM-DD form"
               e.id e.expires)
      | Some _ -> (
          match parse_date today_value with
          | Some _ ->
              if String.compare e.expires today_value <= 0 then
                add "exception.expires" path line
                  (Printf.sprintf
                     "exception '%s' expired on %s; expiry must be after today \
                      (%s)"
                     e.id e.expires today_value)
          | None -> ()))
    src.exceptions;

  (* Skills *)
  List.iter
    (fun (skill : skill) ->
      let path = skill.source in
      let dir_name =
        match String.split_on_char '/' skill.source with
        | "skills" :: name :: _ -> name
        | _ -> ""
      in
      if not (is_kebab_case skill.name) || skill.name <> dir_name then
        add "skill.name" path None
          (Printf.sprintf
             "skill name '%s' must be kebab-case and equal its directory name \
              '%s'"
             skill.name dir_name);
      if String.trim skill.description = "" then
        add "skill.description" path None
          (Printf.sprintf "skill '%s' needs a description" skill.name);
      let require_list field code items =
        if items = [] then
          add code path None
            (Printf.sprintf "skill '%s' needs a non-empty %s list" skill.name
               field)
      in
      require_list "triggers" "skill.triggers" skill.triggers;
      require_list "prerequisites" "skill.prerequisites" skill.prerequisites;
      require_list "stop_conditions" "skill.stop-conditions"
        skill.stop_conditions;
      require_list "verification" "skill.verification" skill.verification;
      if skill.rules = [] then
        add "skill.rules.empty" path None
          (Printf.sprintf "skill '%s' must reference at least one rule"
             skill.name)
      else
        List.iter
          (fun rule_id ->
            if find_any rule_id = None then
              add "skill.rules.target" path None
                (Printf.sprintf
                   "skill '%s' references unknown rule '%s' (broken skill \
                    reference)"
                   skill.name rule_id))
          skill.rules;
      if String.trim skill.body = "" then
        add "skill.body" path None
          (Printf.sprintf "skill '%s' needs a non-empty Markdown body"
             skill.name);
      let has_scripts =
        List.exists
          (fun (rel, _) -> rel = "scripts" || starts_with ~prefix:"scripts/" rel)
          skill.files
      in
      let has_tests =
        List.exists
          (fun (rel, _) -> rel = "tests" || starts_with ~prefix:"tests/" rel)
          skill.files
      in
      if has_scripts && not has_tests then
        add "skill.tests" path None
          (Printf.sprintf
             "skill '%s' ships scripts/ but no tests/ fixtures" skill.name);
      List.iter
        (fun (rel, _) ->
          let full = dir_name ^ "/" ^ rel in
          let full = "skills/" ^ full in
          match read_rel src.root full with
          | Error () ->
              add "source.read" full None "cannot read installed skill file"
          | Ok text ->
              if has_private_absolute_path text then
                add "skill.absolute-path" full None
                  "installed skill file contains a private absolute path";
              secret_findings text
              |> List.iter (fun code ->
                     add code full None
                       (match code with
                       | "secret.private-key" ->
                           "installed skill file contains a PEM private-key header"
                       | "secret.aws-key" ->
                           "installed skill file contains an AWS access-key shape"
                       | _ ->
                           "installed skill file contains an apparent credential \
                            assignment; use an environment-variable reference")))
        skill.files)
    src.skills;

  (* Secrets in policy, rule bodies, and role text *)
  let scan_text path line text =
    secret_findings text
    |> List.iter (fun code ->
           add code path line
             (match code with
             | "secret.private-key" -> "text contains a PEM private-key header"
             | "secret.aws-key" -> "text contains an AWS access-key shape"
             | _ ->
                 "text contains an apparent credential assignment; use an \
                  environment-variable reference"))
  in
  (match src.policy.description with
  | Some text -> scan_text src.policy.source None text
  | None -> ());
  List.iter (fun (r : rule) -> scan_text r.source (Some r.line) r.body) src.rules;
  List.iter
    (fun (role : role) ->
      scan_text role.source None
        (String.concat "\n"
           [ role.description; role.charter; role.output_contract ]))
    src.roles;

  (* Budgets *)
  let core_bytes =
    List.fold_left
      (fun total (r : rule) ->
        if r.layer = Organization && r.kind = Instruction then
          total + String.length r.body
        else total)
      0 src.rules
  in
  if core_bytes > src.policy.core_max_bytes then
    add "budget.core" src.policy.source None
      (Printf.sprintf
         "organization instruction core is %d bytes, over the %d byte budget"
         core_bytes src.policy.core_max_bytes);
  let organization_rules =
    List.filter
      (fun (rule : rule) -> rule.layer = Organization)
      src.rules
  in
  if List.length organization_rules > src.policy.max_rules then
    add "budget.rules" src.policy.source None
      (Printf.sprintf "%d organization rules exceeds the %d rule budget"
         (List.length organization_rules) src.policy.max_rules);

  match normalize_diagnostics !ds with [] -> Ok () | ds -> Error ds
  [@@warning "-16"]

let load_and_validate ~root ?today =
  (* Because [?today] is the final argument, a caller that wants the
     environment/UTC default passes [?today:None]. Callers that already have a
     [source] can use [load] + [validate] instead. *)
  match load ~root with
  | Error ds -> Error ds
  | Ok src -> (
      match validate ?today src with
      | Ok () -> Ok src
      | Error ds -> Error ds)
  [@@warning "-16"]
