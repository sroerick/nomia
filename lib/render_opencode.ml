(** OpenCode v2 configuration.

    Only controls named by the selected adapter's capability matrix are
    emitted. OpenCode v2 [permissions] can express both a read denial and an
    interactive [ask], so the [opencode] adapter honestly supplies the
    [permissions] consent control. The renderer uses it for a deliberately
    small, conservative table of high-impact shell and external-directory
    actions; it does not invent a general shell parser, and the rule-level
    [host-hook] control remains unsupported in explain.

    Permission arrays are ordered and OpenCode applies the last matching rule.
    The [ask] entries therefore come first and the credential [read] denials
    come last, so a deny can never be shadowed by an ask. *)

let schema_url = "https://opencode.ai/config.json"

(** Translate a policy guard path into the resource glob OpenCode matches
    against. The seed corpus uses [**/] prefixes for "at any depth"; OpenCode's
    permission resources name the shape directly. *)
let resource_of_path path =
  (* OpenCode matches the whole normalized path and [*] crosses [/]. Translate
     a policy glob into a pattern that matches both repository-relative and
     absolute forms: [**/foo] and [/home/u/foo] both need a leading [*]. *)
  let path =
    if String.length path > 3 && String.sub path 0 3 = "**/" then
      String.sub path 3 (String.length path - 3)
    else path
  in
  if String.length path > 0 && path.[0] = '*' then path else "*" ^ path

let controls_for policy_set cls =
  List.assoc_opt cls policy_set.Compose.adapter.Model.controls
  |> Option.value ~default:[]

let permission_supported (policy_set : Compose.t) =
  List.mem "permissions" (controls_for policy_set "prohibition")

let consent_supported (policy_set : Compose.t) =
  List.mem "permissions" (controls_for policy_set "consent")

let authority_actions =
  [ "vcs.push"; "deploy.publish"; "shared.mutate"; "fs.delete"; "vcs.history" ]

(** Only claim the native ask when the composed policy actually carries a
    consent rule for one of the authority actions. *)
let authority_consent_present (policy_set : Compose.t) =
  List.exists
    (fun (resolved : Compose.resolved_rule) ->
      match resolved.rule.Model.enforcement with
      | Some enforcement when enforcement.cls = Model.Consent ->
          List.exists
            (fun action -> List.mem action authority_actions)
            resolved.rule.Model.actions
      | _ -> false)
    policy_set.Compose.rules

(** Conservative native approximation of the high-impact actions named by the
    authority rule. Each pair is (OpenCode action, resource glob). *)
let asked_actions =
  [
    ("shell", "git push *");
    ("shell", "rm -rf *");
    ("shell", "sudo *");
    ("external_directory", "*");
  ]

let denied_read_paths (policy_set : Compose.t) =
  if not (permission_supported policy_set) then []
  else
    policy_set.Compose.rules
    |> List.filter_map (fun (resolved : Compose.resolved_rule) ->
           match resolved.rule.Model.enforcement with
           | Some enforcement
             when enforcement.cls = Model.Prohibition
                  && List.mem "permissions" enforcement.controls ->
               Some resolved.rule
           | _ -> None)
    |> List.concat_map (fun (rule : Model.rule) ->
           rule.guards
           |> List.concat_map (fun (guard : Model.guard) ->
                  if guard.action <> "fs.read" then []
                  else List.map resource_of_path guard.paths))
    |> List.sort_uniq String.compare

let permission_json action resource effect_ =
  `Assoc
    [
      ("action", `String action);
      ("resource", `String resource);
      ("effect", `String effect_);
    ]

let ask_permissions policy_set =
  if not (consent_supported policy_set && authority_consent_present policy_set)
  then []
  else
    List.map
      (fun (action, resource) -> permission_json action resource "ask")
      asked_actions

let denied_permissions policy_set =
  List.map
    (fun resource -> permission_json "read" resource "deny")
    (denied_read_paths policy_set)

let config_json (policy_set : Compose.t) =
  `Assoc
    [
      ("$schema", `String schema_url);
      ( "permissions",
        `List (ask_permissions policy_set @ denied_permissions policy_set) );
    ]

(* Stable, human-readable JSON. Object keys keep the order the caller chose
   (so [$schema] is first) rather than being sorted into an opaque blob. *)
let quote value = Yojson.Safe.to_string (`String value)

let rec pretty buffer indent = function
  | `Assoc [] -> Buffer.add_string buffer "{}"
  | `Assoc fields ->
      Buffer.add_string buffer "{\n";
      List.iteri
        (fun index (key, value) ->
          if index > 0 then Buffer.add_string buffer ",\n";
          Buffer.add_string buffer (String.make (indent + 2) ' ');
          Buffer.add_string buffer (quote key);
          Buffer.add_string buffer ": ";
          pretty buffer (indent + 2) value)
        fields;
      Buffer.add_char buffer '\n';
      Buffer.add_string buffer (String.make indent ' ');
      Buffer.add_char buffer '}'
  | `List [] -> Buffer.add_string buffer "[]"
  | `List values ->
      Buffer.add_string buffer "[\n";
      List.iteri
        (fun index value ->
          if index > 0 then Buffer.add_string buffer ",\n";
          Buffer.add_string buffer (String.make (indent + 2) ' ');
          pretty buffer (indent + 2) value)
        values;
      Buffer.add_char buffer '\n';
      Buffer.add_string buffer (String.make indent ' ');
      Buffer.add_char buffer ']'
  | scalar -> Buffer.add_string buffer (Yojson.Safe.to_string scalar)

let config (policy_set : Compose.t) =
  let buffer = Buffer.create 512 in
  pretty buffer 0 (config_json policy_set);
  Buffer.add_char buffer '\n';
  Buffer.contents buffer
