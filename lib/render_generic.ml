(** The shared AGENTS.md layout for the generic and OpenCode clients.

    Both clients consume the same organization core, trusted repository
    fragment, role charter, and skill pointers. Only the installed skill
    directory differs, and that is named by the selected adapter. *)

let strength_name = function
  | Model.Must -> "must"
  | Model.Should -> "should"
  | Model.May -> "may"

let one_line text =
  String.split_on_char '\n' text
  |> List.map String.trim
  |> List.filter (fun line -> line <> "")
  |> String.concat " "

let rule_heading (rule : Model.rule) =
  let strength = strength_name rule.strength in
  if rule.final then Printf.sprintf "## %s (%s, final)" rule.id strength
  else Printf.sprintf "## %s (%s)" rule.id strength

let rule_block (rule : Model.rule) =
  rule_heading rule ^ "\n" ^ String.trim rule.body

let section heading parts =
  String.concat "\n\n" (heading :: parts)

let full_agents_md (policy_set : Compose.t) =
  let release = policy_set.Compose.source.Model.policy.release in
  let header =
    Printf.sprintf
      "<!-- nomia:generated release=%s digest=%s -->\n\
       <!-- Edit the reviewed nomia source, not this generated file. -->"
      release policy_set.Compose.digest
  in
  let rules = policy_set.Compose.rules in
  let by_origin origin =
    List.filter
      (fun (resolved : Compose.resolved_rule) -> resolved.origin = origin)
      rules
  in
  let blocks = ref [ header ] in
  let add block = blocks := block :: !blocks in
  let organization = by_origin Compose.Organization in
  if organization <> [] then
    add
      (section
         (Printf.sprintf "# Organization agent policy (release %s)" release)
         (List.map (fun resolved -> rule_block resolved.Compose.rule) organization));
  List.iter
    (fun (fragment : Compose.fragment) ->
      let parts =
        (if String.trim fragment.Compose.prose = "" then []
         else [ String.trim fragment.prose ])
        @ List.map
            (fun resolved -> rule_block resolved.Compose.rule)
            (List.filter
               (fun (resolved : Compose.resolved_rule) ->
                 resolved.Compose.origin = Compose.Repository
                 && resolved.Compose.rule.Model.source = fragment.Compose.path)
               rules)
      in
      if parts <> [] then
        add
          (section
             (Printf.sprintf "# Repository instructions (%s)"
                fragment.Compose.path)
             parts))
    policy_set.Compose.fragments;
  (match policy_set.Compose.role with
  | None -> ()
  | Some role ->
      let parts =
        [ String.trim role.Model.charter; "## Output contract"; String.trim role.output_contract ]
        @ List.map
            (fun resolved -> rule_block resolved.Compose.rule)
            (by_origin Compose.Role)
      in
      add (section (Printf.sprintf "# Role: %s" role.name) parts));
  if policy_set.Compose.skills <> [] then begin
    let install = policy_set.Compose.adapter.Model.skills.dir in
    let pointers =
      policy_set.Compose.skills
      |> List.map (fun (skill : Model.skill) ->
             Printf.sprintf "- `%s` — %s (installed at %s/%s)" skill.name
               (one_line skill.description) install skill.name)
    in
    add (section "# Skills" [ String.concat "\n" pointers ])
  end;
  String.concat "\n\n" (List.rev !blocks) ^ "\n"

let project_overlay_md (policy_set : Compose.t) =
  let release = policy_set.Compose.source.Model.policy.release in
  let header =
    Printf.sprintf
      "<!-- nomia:generated release=%s digest=%s inherit-root=true -->\n\
       <!-- Edit the reviewed nomia project source, not this generated file. -->"
      release policy_set.Compose.digest
  in
  let blocks = ref [ header ] in
  let add block = blocks := block :: !blocks in
  List.iter
    (fun (fragment : Compose.fragment) ->
      if fragment.scope = Compose.Project then
        let parts =
          (if String.trim fragment.prose = "" then []
           else [ String.trim fragment.prose ])
          @ List.filter_map
              (fun (resolved : Compose.resolved_rule) ->
                if
                  resolved.Compose.origin = Compose.Repository
                  && resolved.Compose.rule.Model.source = fragment.path
                then Some (rule_block resolved.rule)
                else None)
              policy_set.Compose.rules
        in
        if parts <> [] then
          add
            (section
               (Printf.sprintf "# Project instructions (%s)" fragment.path)
               parts))
    policy_set.Compose.fragments;
  (match policy_set.Compose.role with
  | None -> ()
  | Some role ->
      let parts =
        [
          String.trim role.Model.charter;
          "## Output contract";
          String.trim role.output_contract;
        ]
        @ List.filter_map
            (fun (resolved : Compose.resolved_rule) ->
              if resolved.Compose.origin = Compose.Role then
                Some (rule_block resolved.rule)
              else None)
            policy_set.Compose.rules
      in
      add (section (Printf.sprintf "# Role: %s" role.name) parts));
  String.concat "\n\n" (List.rev !blocks) ^ "\n"

let agents_md policy_set =
  if policy_set.Compose.inherit_root then project_overlay_md policy_set
  else full_agents_md policy_set
