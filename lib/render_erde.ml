(** Erde's stable framework segment.

    Erde composes one framework from the organization core, the shared
    harness and memory-lifecycle contract, the selected role charter, then
    approved repository additions and skill procedure pointers. The framework
    is the only stable policy text; Erde keeps claims, project observations,
    artifacts, and the task in its existing separate volatility segments.

    The shared memory contract is a reviewed source fragment, not a string
    copied into this renderer. It is read from
    [fragments/erde-memory-contract.md] and checked against the hash recorded
    by {!Source.load}; a missing, changed, or unhashed fragment is a render
    diagnostic. A separate test proves the fragment bytes equal Erde's
    [Prompt.memory_policy_v1] raw string. *)

let fragment_rel = "fragments/erde-memory-contract.md"

let diagnostic code path message : Model.diagnostic =
  { code; path; line = None; message }

let generated_header (policy_set : Compose.t) =
  let release = policy_set.Compose.source.Model.policy.release in
  Printf.sprintf
    "<!-- nomia:generated release=%s digest=%s -->\n\
     <!-- Edit the reviewed nomia source, not this generated file. -->"
    release policy_set.Compose.digest

let by_origin (policy_set : Compose.t) origin =
  List.filter
    (fun (resolved : Compose.resolved_rule) -> resolved.origin = origin)
    policy_set.Compose.rules

let rule_block (resolved : Compose.resolved_rule) =
  Render_generic.rule_block resolved.Compose.rule

let organization_section (policy_set : Compose.t) =
  let release = policy_set.Compose.source.Model.policy.release in
  Render_generic.section
    (Printf.sprintf "# Organization agent policy (release %s)" release)
    (List.map rule_block (by_origin policy_set Compose.Organization))

let org_core (policy_set : Compose.t) =
  String.concat "\n\n" [ generated_header policy_set; organization_section policy_set ]
  ^ "\n"

let memory_contract (policy_set : Compose.t) =
  let source = policy_set.Compose.source in
  let absolute = Filename.concat source.Model.root fragment_rel in
  match In_channel.with_open_bin absolute In_channel.input_all with
  | contents ->
      if String.trim contents = "" then
        Error
          [
            diagnostic "render.fragment.empty" fragment_rel
              "the reviewed memory contract fragment is empty";
          ]
      else
        let actual = Source.sha256_hex contents in
        (match List.assoc_opt fragment_rel source.Model.files with
        | None ->
            Error
              [
                diagnostic "render.fragment.unhashed" fragment_rel
                  "the memory contract fragment is not part of the loaded \
                   source; fragments/*.md must be hashed into source.files";
              ]
        | Some expected when expected <> actual ->
            Error
              [
                diagnostic "render.fragment.hash" fragment_rel
                  (Printf.sprintf
                     "memory contract fragment changed after it was loaded: \
                      expected sha256 %s, found %s"
                     expected actual);
              ]
        | Some _ -> Ok contents)
  | exception Sys_error _ ->
      Error
        [
          diagnostic "render.fragment.read" fragment_rel
            "cannot read the reviewed memory contract fragment while rendering";
        ]

let role_section (policy_set : Compose.t) =
  match policy_set.Compose.role with
  | None -> None
  | Some role ->
      Some
        (Render_generic.section
           (Printf.sprintf "## Role: %s" role.Model.name)
           [
             String.trim role.Model.charter;
             "### Output contract";
             String.trim role.Model.output_contract;
           ])

let repository_sections (policy_set : Compose.t) =
  policy_set.Compose.fragments
  |> List.filter_map (fun (fragment : Compose.fragment) ->
         let parts =
           (if String.trim fragment.Compose.prose = "" then []
            else [ String.trim fragment.Compose.prose ])
           @ List.filter_map
               (fun (resolved : Compose.resolved_rule) ->
                 if
                   resolved.Compose.origin = Compose.Repository
                   && resolved.Compose.rule.Model.source = fragment.Compose.path
                 then Some (rule_block resolved)
                 else None)
               policy_set.Compose.rules
         in
         if parts = [] then None
         else
           Some
             (Render_generic.section
                (Printf.sprintf "## Repository instructions (%s)"
                   fragment.Compose.path)
                parts))

let role_additions_section (policy_set : Compose.t) =
  match by_origin policy_set Compose.Role with
  | [] -> None
  | additions ->
      Some
        (Render_generic.section "## Role additions"
           (List.map rule_block additions))

let procedures_section (policy_set : Compose.t) =
  if policy_set.Compose.skills = [] then None
  else
    let install = policy_set.Compose.adapter.Model.skills.dir in
    let pointers =
      policy_set.Compose.skills
      |> List.map (fun (skill : Model.skill) ->
             Printf.sprintf "- `%s` — %s (installed at %s/%s)" skill.name
               (Render_generic.one_line skill.description)
               install skill.name)
    in
    Some
      (Render_generic.section "# Procedures" [ String.concat "\n" pointers ])

let framework (policy_set : Compose.t) ~memory_contract =
  let parts =
    [
      generated_header policy_set;
      organization_section policy_set;
      Render_generic.section
        "## Shared harness and memory lifecycle contract"
        [ memory_contract ];
    ]
  in
  let optional =
    List.filter_map Fun.id [ role_section policy_set ]
    @ repository_sections policy_set
    @ List.filter_map Fun.id
        [ role_additions_section policy_set; procedures_section policy_set ]
  in
  String.concat "\n\n" (parts @ optional) ^ "\n"

let role_json (policy_set : Compose.t) =
  match policy_set.Compose.role with
  | None -> `Null
  | Some role ->
      let source_hash =
        List.assoc_opt role.Model.source policy_set.Compose.source.Model.files
        |> Option.value ~default:""
      in
      `Assoc
        [
          ("name", `String role.Model.name);
          ("role_sha256", `String source_hash);
          ("charter_sha256", `String (Source.sha256_hex role.Model.charter));
          ( "output_contract_sha256",
            `String (Source.sha256_hex role.Model.output_contract) );
        ]

let policy_json (policy_set : Compose.t) ~memory_contract_text ~framework_text =
  let source = policy_set.Compose.source in
  let json =
    `Assoc
      [
        ("schema", `String "nomia-erde/v1");
        ("release", `String source.Model.policy.release);
        ("compiler", `String source.Model.policy.compiler);
        ("policy_digest", `String policy_set.Compose.digest);
        ("source_digest", `String policy_set.Compose.source_digest);
        ( "org_core_sha256",
          `String (Source.sha256_hex (org_core policy_set)) );
        ( "memory_contract_sha256",
          `String (Source.sha256_hex memory_contract_text) );
        ("framework_sha256", `String (Source.sha256_hex framework_text));
        ("role", role_json policy_set);
      ]
  in
  Canonical.to_string json ^ "\n"
