(** Saguaro's managed persona.

    Saguaro's persona template is reviewed and installed as a source fragment,
    not copied into this renderer. The template keeps every [{{OWNER}}],
    [{{AGENT}}], and [{{HOST}}] placeholder: the Saguaro generator fills them
    at first boot and the generator, not this renderer, owns personalization.

    Rendering inserts one generated organization baseline block immediately
    before the template's [## Voice] heading. The persona voice text is left
    byte-for-byte intact, and [policy.json] records the release digest and the
    exact template/persona hashes. *)

let fragment_rel = "fragments/saguaro-persona-template.md"

let diagnostic code path message : Model.diagnostic =
  { code; path; line = None; message }

let generated_header (policy_set : Compose.t) =
  let release = policy_set.Compose.source.Model.policy.release in
  Printf.sprintf
    "<!-- nomia:generated release=%s digest=%s -->\n\
     <!-- Edit the reviewed nomia source, not this generated persona. -->"
    release policy_set.Compose.digest

let by_origin (policy_set : Compose.t) origin =
  List.filter
    (fun (resolved : Compose.resolved_rule) -> resolved.origin = origin)
    policy_set.Compose.rules

let rule_block (resolved : Compose.resolved_rule) =
  Render_generic.rule_block resolved.Compose.rule

let policy_block (policy_set : Compose.t) =
  let release = policy_set.Compose.source.Model.policy.release in
  let rules = List.map rule_block (by_origin policy_set Compose.Organization) in
  let parts =
    [
      generated_header policy_set;
      Printf.sprintf "## Organization baseline (release %s)" release;
    ]
    @ rules
  in
  String.concat "\n\n" parts

let find_heading text heading =
  let text_length = String.length text and heading_length = String.length heading in
  let rec loop index =
    if index + heading_length > text_length then None
    else if
      String.sub text index heading_length = heading
      && (index = 0 || text.[index - 1] = '\n')
      && (index + heading_length = text_length
         || text.[index + heading_length] = '\n'
         || text.[index + heading_length] = '\r')
    then Some index
    else loop (index + 1)
  in
  loop 0

let template (policy_set : Compose.t) =
  let source = policy_set.Compose.source in
  let absolute = Filename.concat source.Model.root fragment_rel in
  match In_channel.with_open_bin absolute In_channel.input_all with
  | contents ->
      if String.trim contents = "" then
        Error
          [
            diagnostic "render.fragment.empty" fragment_rel
              "the reviewed Saguaro persona template is empty";
          ]
      else
        let actual = Source.sha256_hex contents in
        (match List.assoc_opt fragment_rel source.Model.files with
        | None ->
            Error
              [
                diagnostic "render.fragment.unhashed" fragment_rel
                  "the Saguaro persona template is not part of the loaded \
                   source; fragments/*.md must be hashed into source.files";
              ]
        | Some expected when expected <> actual ->
            Error
              [
                diagnostic "render.fragment.hash" fragment_rel
                  (Printf.sprintf
                     "Saguaro persona template changed after it was loaded: \
                      expected sha256 %s, found %s"
                     expected actual);
              ]
        | Some _ -> Ok contents)
  | exception Sys_error _ ->
      Error
        [
          diagnostic "render.fragment.read" fragment_rel
            "cannot read the reviewed Saguaro persona template while rendering";
        ]

let persona (policy_set : Compose.t) ~template_text =
  let block = policy_block policy_set in
  match find_heading template_text "## Voice" with
  | Some index ->
      let prefix = String.sub template_text 0 index in
      let suffix =
        String.sub template_text index (String.length template_text - index)
      in
      Ok (prefix ^ block ^ "\n\n" ^ suffix)
  | None ->
      Error
        [
          diagnostic "render.saguaro.voice-heading" fragment_rel
            "the reviewed Saguaro persona template has no '## Voice' heading; \
             refusing to place the organization baseline silently";
        ]

let role_json (policy_set : Compose.t) =
  match policy_set.Compose.role with
  | None -> `Null
  | Some role -> `String role.Model.name

let policy_json (policy_set : Compose.t) ~template_text ~persona_text =
  let source = policy_set.Compose.source in
  let json =
    `Assoc
      [
        ("schema", `String "nomia-saguaro/v1");
        ("release", `String source.Model.policy.release);
        ("compiler", `String source.Model.policy.compiler);
        ("policy_digest", `String policy_set.Compose.digest);
        ("source_digest", `String policy_set.Compose.source_digest);
        ("template_sha256", `String (Source.sha256_hex template_text));
        ("persona_sha256", `String (Source.sha256_hex persona_text));
        ("personalized", `Bool false);
        ("role", role_json policy_set);
      ]
  in
  Canonical.to_string json ^ "\n"
