open Nomia

module S = Nomia.Source
module M = Nomia.Model

open M

(* ------------------------------------------------------------------ *)
(* Helpers                                                            *)
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

let is_hex c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

let line_string = function None -> "-" | Some n -> string_of_int n

let pp_diag fmt (d : M.diagnostic) =
  Format.fprintf fmt "%s:%s:%s (%s)" d.path (line_string d.line) d.code d.message

let diag_testable = Alcotest.testable pp_diag ( = )

let string_of_diagnostics ds =
  String.concat "; " (List.map (fun d -> Format.asprintf "%a" pp_diag d) ds)

let codes ds = List.map (fun (d : M.diagnostic) -> d.code) ds

let expect_ok name = function
  | Ok value -> value
  | Error ds ->
      Alcotest.failf "%s: unexpected diagnostics: %s" name
        (string_of_diagnostics ds)

let expect_error name code = function
  | Ok _ -> Alcotest.failf "%s: expected diagnostic '%s', got success" name code
  | Error ds ->
      Alcotest.(check bool)
        (name ^ " reports " ^ code)
        true
        (List.mem code (codes ds));
      ds

let read_file path =
  In_channel.with_open_bin path In_channel.input_all

let valid_today = "2026-01-01"

let load_ok root = expect_ok ("load " ^ root) (S.load ~root)

let validate_ok ?(today = valid_today) src =
  expect_ok "validate" (S.validate ~today src)

let load_and_validate_ok ?(today = valid_today) root =
  expect_ok ("load_and_validate " ^ root)
    (S.load_and_validate ~root ~today)

let compare_diagnostic (a : M.diagnostic) b =
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

let check_sorted name ds =
  Alcotest.(check (list diag_testable)) name (List.sort compare_diagnostic ds) ds

let check_path_present name path ds =
  Alcotest.(check bool)
    (name ^ " names " ^ path)
    true
    (List.exists (fun (d : M.diagnostic) -> d.path = path) ds)

let check_relative_paths name ds =
  Alcotest.(check bool)
    (name ^ " uses relative paths only")
    true
    (List.for_all
       (fun (d : M.diagnostic) ->
         d.path <> "" && d.path.[0] <> '/' && not (contains_sub d.path "\\"))
       ds)

let hash_ok hash =
  String.length hash = 64 && String.for_all is_hex hash

(* ------------------------------------------------------------------ *)
(* Real corpus                                                        *)
(* ------------------------------------------------------------------ *)

let test_real_corpus_loads () =
  let src = load_ok ".." in
  Alcotest.(check string) "policy schema" "nomia/v1" src.policy.schema;
  Alcotest.(check string) "policy release" "0.1.0" src.policy.release;
  Alcotest.(check string) "policy compiler" "agentctl/1" src.policy.compiler;
  Alcotest.(check int) "policy maintainers" 3
    (List.length src.policy.maintainers);
  Alcotest.(check int) "rule count" 11 (List.length src.rules);
  Alcotest.(check int) "role count" 2 (List.length src.roles);
  Alcotest.(check int) "adapter count" 4 (List.length src.adapters);
  Alcotest.(check int) "skill count" 3 (List.length src.skills);
  Alcotest.(check bool) "source files present" true (src.files <> []);
  Alcotest.(check bool) "at least 20 source files" true
    (List.length src.files >= 20);
  let paths = List.map fst src.files in
  Alcotest.(check (list string)) "source file paths sorted" (List.sort String.compare paths) paths;
  let rule_sources = List.map (fun (r : M.rule) -> r.source) src.rules in
  Alcotest.(check (list string)) "rule sources sorted"
    (List.sort String.compare rule_sources)
    rule_sources;
  Alcotest.(check bool) "every file hash is sha256 hex" true
    (List.for_all (fun (_, hash) -> hash_ok hash) src.files);
  Alcotest.(check bool) "all loaded paths are relative" true
    (List.for_all
       (fun (path, _) -> path <> "" && path.[0] <> '/')
       src.files);
  Alcotest.(check bool) "no rule points outside rules/" true
    (List.for_all (fun (r : M.rule) -> contains_sub r.source "rules/") src.rules);
  validate_ok src;
  let validated = load_and_validate_ok ".." in
  Alcotest.(check int) "load_and_validate returns the same rules" 11
    (List.length validated.rules)

let test_sha256_hex () =
  Alcotest.(check string) "sha256 abc"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (S.sha256_hex "abc");
  Alcotest.(check string) "sha256 empty"
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (S.sha256_hex "");
  Alcotest.(check int) "sha256 hex length" 64 (String.length (S.sha256_hex "nomia"))

let test_determinism () =
  let a = load_ok ".." and b = load_ok ".." in
  Alcotest.(check (list (pair string string)))
    "file digests are identical across loads" a.files b.files;
  let ids src = List.map (fun (r : M.rule) -> r.id) src.rules in
  Alcotest.(check (list string)) "rule ids are identical" (ids a) (ids b);
  let skills src = List.map (fun (s : M.skill) -> s.name) src.skills in
  Alcotest.(check (list string)) "skill names are identical" (skills a) (skills b);
  let roles src = List.map (fun (r : M.role) -> r.name) src.roles in
  Alcotest.(check (list string)) "role names are identical" (roles a) (roles b);
  let adapters src = List.map (fun (a : M.adapter) -> a.id) src.adapters in
  Alcotest.(check (list string)) "adapter ids are identical" (adapters a) (adapters b);
  Alcotest.(check int) "rule record count is stable" (List.length a.rules)
    (List.length b.rules)

(* ------------------------------------------------------------------ *)
(* Invalid fixtures                                                   *)
(* ------------------------------------------------------------------ *)

let test_duplicate_id () =
  let ds =
    expect_error "duplicate-id" "rule.duplicate-id"
      (S.load_and_validate ~root:"fixtures/invalid/duplicate-id" ~today:valid_today)
  in
  check_relative_paths "duplicate-id" ds;
  check_path_present "duplicate-id" "rules/b.md" ds

let test_missing_owner () =
  let ds =
    expect_error "missing-owner" "rule.owner.missing"
      (S.load_and_validate ~root:"fixtures/invalid/missing-owner" ~today:valid_today)
  in
  check_path_present "missing-owner" "rules/core.md" ds

let test_placeholder_owner () =
  expect_error "placeholder-owner" "rule.owner.placeholder"
    (S.load_and_validate ~root:"fixtures/invalid/placeholder-owner"
       ~today:valid_today)
  |> ignore

let test_unknown_field () =
  let ds =
    expect_error "unknown-field" "rule.unknown-field"
      (S.load_and_validate ~root:"fixtures/invalid/unknown-field" ~today:valid_today)
  in
  check_relative_paths "unknown-field" ds

let test_duplicate_key () =
  expect_error "duplicate-key" "yaml.parse"
    (S.load_and_validate ~root:"fixtures/invalid/duplicate-key" ~today:valid_today)
  |> ignore

let test_conflicting_claims () =
  let ds =
    expect_error "conflicting-claims" "rule.key-conflict"
      (S.load_and_validate ~root:"fixtures/invalid/conflicting-claims"
         ~today:valid_today)
  in
  check_relative_paths "conflicting-claims" ds

let test_broken_skill_reference () =
  let ds =
    expect_error "broken-skill-reference" "skill.rules.target"
      (S.load_and_validate ~root:"fixtures/invalid/broken-skill-reference"
         ~today:valid_today)
  in
  check_path_present "broken-skill-reference" "skills/broken/SKILL.md" ds

let test_unauthorized_override () =
  let ds =
    expect_error "unauthorized-override" "role.relaxes.final"
      (S.load_and_validate ~root:"fixtures/invalid/unauthorized-override"
         ~today:valid_today)
  in
  check_path_present "unauthorized-override" "roles/relaxer.yaml" ds

let test_expired_exception () =
  let ds =
    expect_error "expired-exception" "exception.expires"
      (S.load_and_validate ~root:"fixtures/invalid/expired-exception"
         ~today:valid_today)
  in
  check_path_present "expired-exception" "exceptions.yaml" ds

let test_bad_adapter_control () =
  let ds =
    expect_error "bad-adapter-control" "adapter.control"
      (S.load_and_validate ~root:"fixtures/invalid/bad-adapter-control"
         ~today:valid_today)
  in
  check_path_present "bad-adapter-control" "adapters/opencode.yaml" ds

let test_absolute_skill_file () =
  let ds =
    expect_error "absolute-skill-file" "skill.absolute-path"
      (S.load_and_validate ~root:"fixtures/invalid/absolute-skill-file"
         ~today:valid_today)
  in
  check_path_present "absolute-skill-file" "skills/absolute/scripts/run.sh" ds

let test_secret_rule () =
  let ds =
    expect_error "secret-rule" "secret.aws-key"
      (S.load_and_validate ~root:"fixtures/invalid/secret-rule" ~today:valid_today)
  in
  check_path_present "secret-rule" "rules/core.md" ds

let test_sorted_diagnostics () =
  let ds =
    expect_error "sorted-errors" "exception.expires"
      (S.load_and_validate ~root:"fixtures/invalid/sorted-errors"
         ~today:valid_today)
  in
  check_sorted "diagnostics sorted by path, line, code" ds;
  Alcotest.(check (list string)) "sorted diagnostic paths"
    [ "exceptions.yaml"; "rules/a.md"; "rules/z.md" ]
    (List.map (fun (d : M.diagnostic) -> d.path) ds);
  Alcotest.(check bool) "sorted-errors has more than one diagnostic" true
    (List.length ds >= 3)

(* ------------------------------------------------------------------ *)
(* Valid fixtures and model shape                                     *)
(* ------------------------------------------------------------------ *)

let test_minimal_valid () =
  let src = load_and_validate_ok "fixtures/valid/minimal" in
  Alcotest.(check string) "minimal schema" "nomia/v1" src.policy.schema;
  Alcotest.(check int) "minimal rule count" 1 (List.length src.rules);
  Alcotest.(check int) "minimal role count" 1 (List.length src.roles);
  Alcotest.(check int) "minimal adapter count" 1 (List.length src.adapters);
  Alcotest.(check int) "minimal skill count" 1 (List.length src.skills);
  Alcotest.(check int) "minimal exception count" 0
    (List.length src.exceptions);
  let rule = List.hd src.rules in
  Alcotest.(check string) "minimal rule id" "test.core" rule.id;
  Alcotest.(check bool) "minimal rule is organization layer" true
    (rule.layer = M.Organization);
  Alcotest.(check bool) "minimal rule has enforcement" true
    (rule.enforcement <> None);
  let role = List.hd src.roles in
  Alcotest.(check string) "role memory contract" "shared"
    role.memory_contract;
  Alcotest.(check (list string)) "role additions" [ "test.core" ]
    role.additions;
  let adapter = List.hd src.adapters in
  Alcotest.(check string) "adapter id" "opencode" adapter.id;
  Alcotest.(check int) "adapter control classes" 4
    (List.length adapter.controls);
  Alcotest.(check bool) "adapter has advisory controls" true
    (List.assoc_opt "advisory" adapter.controls
    = Some [ "instruction"; "skill" ]);
  let skill = List.hd src.skills in
  Alcotest.(check (list string)) "skill rules" [ "test.core" ] skill.rules;
  Alcotest.(check bool) "skill includes SKILL.md in its file unit" true
    (List.mem_assoc "SKILL.md" skill.files);
  Alcotest.(check bool) "skill SKILL.md hash is sha256" true
    (hash_ok (List.assoc "SKILL.md" skill.files));
  Alcotest.(check bool) "source files contain the skill file" true
    (List.mem_assoc "skills/example/SKILL.md" src.files);
  validate_ok src

let test_validation_is_repeatable () =
  let src = load_ok "fixtures/valid/minimal" in
  validate_ok src;
  validate_ok src;
  Alcotest.(check bool) "validates twice without changing the source" true
    (List.length src.rules = 1)

(* ------------------------------------------------------------------ *)
(* Front matter and fragments                                         *)
(* ------------------------------------------------------------------ *)

let test_parse_frontmatter_offset () =
  let text = read_file "fixtures/fragment/offset.md" in
  let tree, body, offset =
    expect_ok "parse_frontmatter offset"
      (S.parse_frontmatter ~path:"fixtures/fragment/offset.md" text)
  in
  Alcotest.(check int) "body line offset is after the closing ---" 4 offset;
  Alcotest.(check bool) "body starts at the first prose line" true
    (contains_sub body "body starts here");
  Alcotest.(check bool) "second body line is kept" true
    (contains_sub body "second line");
  Alcotest.(check bool) "front matter parses the title" true
    (match Nomia.Yaml_lite.get "title" tree with
    | Some (Nomia.Yaml_lite.String "offset") -> true
    | _ -> false)

let test_parse_frontmatter_absent () =
  let text = "Just prose with no front matter.\n" in
  let tree, body, offset =
    expect_ok "parse_frontmatter absent"
      (S.parse_frontmatter ~path:"prose.md" text)
  in
  Alcotest.(check int) "prose body starts at line 1" 1 offset;
  Alcotest.(check bool) "prose has no front matter tree" true
    (tree = Nomia.Yaml_lite.Null);
  Alcotest.(check string) "prose body is the whole file" text body

let test_parse_frontmatter_unterminated () =
  let ds =
    expect_error "unterminated frontmatter" "frontmatter.unterminated"
      (S.parse_frontmatter ~path:"unterminated.md" "---\nid: test.x\n")
  in
  check_path_present "unterminated frontmatter" "unterminated.md" ds

let test_load_fragment_prose () =
  let rules, body, files =
    expect_ok "fragment prose"
      (S.load_fragment ~path:"fixtures/fragment/prose.md")
  in
  Alcotest.(check int) "prose fragment has no rules" 0 (List.length rules);
  Alcotest.(check bool) "prose fragment body is prose" true
    (contains_sub body "Just prose");
  Alcotest.(check int) "prose fragment has one hashed file" 1
    (List.length files);
  Alcotest.(check bool) "prose fragment hash is sha256" true
    (hash_ok (snd (List.hd files)))

let test_load_fragment_rules () =
  let rules, body, files =
    expect_ok "fragment rules"
      (S.load_fragment ~path:"fixtures/fragment/rules.md")
  in
  Alcotest.(check int) "fragment has two inline rules" 2 (List.length rules);
  Alcotest.(check string) "first fragment rule id" "repo.fragment-one"
    (List.nth rules 0).id;
  Alcotest.(check string) "second fragment rule id" "repo.fragment-two"
    (List.nth rules 1).id;
  Alcotest.(check bool) "fragment rules use the repository layer" true
    (List.for_all (fun (r : M.rule) -> r.layer = M.Repository) rules);
  Alcotest.(check bool) "fragment rule source names the fragment" true
    (List.for_all
       (fun (r : M.rule) -> r.source = "fixtures/fragment/rules.md")
       rules);
  Alcotest.(check string) "fragment body is shared prose"
    "Fragment body used by both inline rules.\n"
    body;
  Alcotest.(check int) "fragment file unit" 1 (List.length files)

let test_load_fragment_relaxes () =
  let rules, _, _ =
    expect_ok "fragment relaxes"
      (S.load_fragment ~path:"fixtures/fragment/relaxes.md")
  in
  Alcotest.(check int) "relaxes fragment has one rule" 1 (List.length rules);
  Alcotest.(check (list string)) "top-level relaxes attaches to inline rules"
    [ "engineering.scope" ]
    (List.hd rules).relaxes

(* ------------------------------------------------------------------ *)
(* Exception expiry and today selection                               *)
(* ------------------------------------------------------------------ *)

let test_exception_expiry () =
  ignore
    (expect_ok "future exception"
       (S.load_and_validate ~root:"fixtures/valid/with-exception"
          ~today:"2999-12-30"));
  expect_error "expiry on the boundary" "exception.expires"
    (S.load_and_validate ~root:"fixtures/valid/with-exception"
       ~today:"2999-12-31")
  |> ignore;
  expect_error "expiry past the boundary" "exception.expires"
    (S.load_and_validate ~root:"fixtures/valid/with-exception"
       ~today:"3000-01-01")
  |> ignore

let test_today_environment () =
  Unix.putenv "NOMIA_TODAY" "3000-01-01";
  expect_error "environment today selects expiry" "exception.expires"
    (S.load_and_validate ~root:"fixtures/valid/with-exception" ?today:None)
  |> ignore;
  Unix.putenv "NOMIA_TODAY" "2999-12-30";
  ignore
    (expect_ok "environment today before expiry"
       (S.load_and_validate ~root:"fixtures/valid/with-exception" ?today:None));
  Unix.putenv "NOMIA_TODAY" "3000-01-01";
  ignore
    (expect_ok "explicit today overrides the environment"
       (S.load_and_validate ~root:"fixtures/valid/with-exception"
          ~today:"2999-12-30"));
  Unix.putenv "NOMIA_TODAY" ""

(* ------------------------------------------------------------------ *)
(* Secret and path false positives                                    *)
(* ------------------------------------------------------------------ *)

let test_secret_false_positives () =
  let src = load_and_validate_ok "fixtures/valid/secret-prose" in
  Alcotest.(check int) "secret prose fixture loads" 1 (List.length src.rules);
  Alcotest.(check bool) "placeholder credentials are not secrets" true
    (not
       (List.exists
          (fun code -> contains_sub code "secret.")
          (codes
             (match S.validate ~today:valid_today src with
             | Ok () -> []
             | Error ds -> ds))));
  Alcotest.(check bool) "the AKIA prefix alone is prose" true
    (contains_sub (List.hd src.rules).body "AKIA prefix alone")

let test_clean_corpus_has_no_secret_diagnostics () =
  match S.load_and_validate ~root:".." ~today:valid_today with
  | Ok _ -> ()
  | Error ds ->
      Alcotest.(check bool)
        "clean corpus has no secret diagnostics"
        false
        (List.exists
           (fun code -> contains_sub code "secret.")
           (codes ds))

(* ------------------------------------------------------------------ *)
(* Direct model validation                                            *)
(* ------------------------------------------------------------------ *)

let base_policy : M.policy =
  {
    schema = "nomia/v1";
    release = "0.1.0";
    compiler = "agentctl/1";
    description = None;
    maintainers = [ ("wyo-engineering", "engineering-owner") ];
    core_max_bytes = 4096;
    max_rules = 16;
    source = "policy.yaml";
  }

let base_rule : M.rule =
  {
    id = "test.core";
    revision = 1;
    owner = "wyo-engineering";
    strength = M.Must;
    scope = [ "all" ];
    final = false;
    kind = M.Instruction;
    title = None;
    enforcement = None;
    actions = [];
    guards = [];
    key = None;
    value = None;
    relaxes = [];
    supersedes = None;
    body = "body\n";
    source = "rules/core.md";
    line = 1;
    layer = M.Organization;
  }

let base_role : M.role =
  {
    name = "implementer";
    description = "Fixture role.";
    charter = "Do the fixture work.";
    output_contract = "Report the fixture work.";
    memory_contract = "shared";
    relaxes = [];
    additions = [];
    source = "roles/implementer.yaml";
  }

let base_adapter : M.adapter =
  {
    id = "opencode";
    version = 1;
    description = "Fixture adapter.";
    instructions =
      { file = "AGENTS.md"; discovery = [ "AGENTS.md" ]; includes = false };
    skills =
      {
        dir = ".opencode/skills";
        auto_discovery = true;
        explicit_reference = true;
      };
    roles = false;
    mcp = false;
    hooks = false;
    permissions = true;
    atomic_output = true;
    controls =
      [
        ("advisory", [ "instruction"; "skill" ]);
        ("gate", [ "ci-verification" ]);
        ("consent", []);
        ("prohibition", [ "permissions" ]);
      ];
    unsupported = [ "none" ];
    source = "adapters/opencode.yaml";
  }

let base_skill : M.skill =
  {
    name = "example";
    description = "Fixture skill.";
    triggers = [ "trigger" ];
    prerequisites = [ "prerequisite" ];
    stop_conditions = [ "stop" ];
    verification = [ "verify" ];
    rules = [ "test.core" ];
    body = "Skill body.\n";
    files = [];
    source = "skills/example/SKILL.md";
  }

let base_exception : M.exception_ =
  {
    id = "exc.fixture";
    rule = "test.core";
    scope = "repo:acme/widget";
    reason = "Fixture reason.";
    approver = "wyo-security";
    expires = "2999-12-31";
    source = "exceptions.yaml";
    line = 1;
  }

let empty_source : M.source =
  {
    root = "fixtures/does-not-exist";
    policy = base_policy;
    rules = [];
    skills = [];
    roles = [];
    adapters = [];
    exceptions = [];
    files = [];
  }

let with_rules rules = { empty_source with rules }
let one_rule rule = with_rules [ rule ]

let expect_validation_error name code src =
  expect_error name code (S.validate ~today:valid_today src) |> ignore

let test_base_model_valid () =
  ignore (expect_ok "base model" (S.validate ~today:valid_today (one_rule base_rule)))

let test_policy_schema () =
  expect_validation_error "policy schema" "policy.schema"
    { (one_rule base_rule) with policy = { base_policy with schema = "nomia/v2" } }

let test_policy_release () =
  expect_validation_error "policy release" "policy.release"
    { (one_rule base_rule) with policy = { base_policy with release = "v1" } }

let test_policy_compiler () =
  expect_validation_error "policy compiler" "policy.compiler"
    { (one_rule base_rule) with policy = { base_policy with compiler = "cli/1" } }

let test_policy_maintainers_empty () =
  expect_validation_error "policy maintainers" "policy.maintainers"
    { (one_rule base_rule) with policy = { base_policy with maintainers = [] } }

let test_policy_maintainer_role () =
  expect_validation_error "policy maintainer role" "policy.maintainers"
    {
      (one_rule base_rule) with
      policy = { base_policy with maintainers = [ ("x", "nope") ] };
    }

let test_policy_budget () =
  expect_validation_error "policy budget" "policy.budget"
    { (one_rule base_rule) with policy = { base_policy with core_max_bytes = 0 } }

let test_rule_id () =
  expect_validation_error "rule id" "rule.id.invalid"
    (one_rule { base_rule with id = "Not.AnId" })

let test_rule_revision () =
  expect_validation_error "rule revision" "rule.revision"
    (one_rule { base_rule with revision = 0 })

let test_rule_owner_missing () =
  expect_validation_error "rule owner" "rule.owner.missing"
    (one_rule { base_rule with owner = "" })

let test_rule_owner_placeholder () =
  expect_validation_error "rule placeholder owner" "rule.owner.placeholder"
    (one_rule { base_rule with owner = "unassigned" })

let test_rule_scope () =
  expect_validation_error "rule scope" "rule.scope"
    (one_rule { base_rule with scope = [] })

let test_rule_action_unknown () =
  expect_validation_error "rule action" "rule.action.unknown"
    (one_rule { base_rule with actions = [ "made.up" ] })

let test_rule_guard_action () =
  expect_validation_error "rule guard action" "rule.guard.action"
    (one_rule
       {
         base_rule with
         guards = [ { action = "made.up"; paths = [ "**/*" ]; hosts = [] } ];
       })

let test_rule_key_value () =
  expect_validation_error "rule key/value" "rule.key-value"
    (one_rule { base_rule with key = Some "claim"; value = None })

let test_rule_supersedes_syntax () =
  expect_validation_error "rule supersedes syntax" "rule.supersedes"
    (one_rule { base_rule with supersedes = Some "test.core" })

let test_rule_supersedes_self () =
  expect_validation_error "rule supersedes self" "rule.supersedes"
    (one_rule { base_rule with revision = 1; supersedes = Some "test.core@2" })

let test_rule_supersedes_existing () =
  let old = { base_rule with id = "test.old"; revision = 5 } in
  let newer = { base_rule with id = "test.new"; supersedes = Some "test.old@2" } in
  expect_validation_error "rule supersedes existing" "rule.supersedes"
    (with_rules [ old; newer ])

let test_rule_supersedes_valid () =
  let old = { base_rule with id = "test.old"; revision = 2 } in
  let newer = { base_rule with id = "test.new"; supersedes = Some "test.old@2" } in
  ignore
    (expect_ok "valid supersession"
       (S.validate ~today:valid_today (with_rules [ old; newer ])))

let test_rule_enforcement_control () =
  expect_validation_error "rule enforcement control" "rule.enforcement.control"
    (one_rule
       {
         base_rule with
         enforcement =
           Some { cls = M.Advisory; controls = [ "ci-verification" ] };
       })

let test_rule_enforcement_controls_final () =
  expect_validation_error "final consent needs a control"
    "rule.enforcement.controls"
    (one_rule
       {
         base_rule with
         final = true;
         enforcement = Some { cls = M.Consent; controls = [] };
       })

let test_rule_final_owner () =
  expect_validation_error "final rule owner" "rule.final-owner"
    (one_rule { base_rule with final = true; owner = "" })

let test_rule_relaxes_org () =
  expect_validation_error "org rule relaxes" "rule.relaxes.org"
    (one_rule { base_rule with relaxes = [ "other.rule" ] })

let test_rule_duplicate_id () =
  expect_validation_error "duplicate rule id" "rule.duplicate-id"
    (with_rules [ base_rule; { base_rule with source = "rules/other.md" } ])

let test_rule_key_conflict () =
  let a = { base_rule with id = "test.a"; key = Some "test.k"; value = Some "one" } in
  let b = { base_rule with id = "test.b"; key = Some "test.k"; value = Some "two" } in
  expect_validation_error "rule key conflict" "rule.key-conflict"
    (with_rules [ a; b ])

let test_rule_relaxes_target () =
  let repo =
    {
      base_rule with
      id = "repo.rule";
      source = "rules/repo.md";
      layer = M.Repository;
      relaxes = [ "missing.rule" ];
    }
  in
  expect_validation_error "repo relaxes unknown" "rule.relaxes.target"
    (one_rule repo)

let test_rule_relaxes_final () =
  let org = { base_rule with id = "org.final"; final = true } in
  let repo =
    {
      base_rule with
      id = "repo.rule";
      source = "rules/repo.md";
      layer = M.Repository;
      relaxes = [ "org.final" ];
    }
  in
  expect_validation_error "repo relaxes final" "rule.relaxes.final"
    (with_rules [ org; repo ])

let test_role_name () =
  expect_validation_error "role name" "role.name"
    { (one_rule base_rule) with roles = [ { base_role with name = "Not_Kebab" } ] }

let test_role_description () =
  expect_validation_error "role description" "role.description"
    { (one_rule base_rule) with roles = [ { base_role with description = "" } ] }

let test_role_charter () =
  expect_validation_error "role charter" "role.charter"
    { (one_rule base_rule) with roles = [ { base_role with charter = "" } ] }

let test_role_output_contract () =
  expect_validation_error "role output contract" "role.output-contract"
    {
      (one_rule base_rule) with
      roles = [ { base_role with output_contract = "" } ];
    }

let test_role_memory_contract () =
  expect_validation_error "role memory contract" "role.memory-contract"
    {
      (one_rule base_rule) with
      roles = [ { base_role with memory_contract = "private" } ];
    }

let test_role_relaxes_target () =
  expect_validation_error "role relaxes unknown" "role.relaxes.target"
    {
      (one_rule base_rule) with
      roles = [ { base_role with relaxes = [ "missing.rule" ] } ];
    }

let test_role_relaxes_final () =
  let org = { base_rule with id = "org.final"; final = true } in
  expect_validation_error "role relaxes final" "role.relaxes.final"
    {
      (with_rules [ org ]) with
      roles = [ { base_role with relaxes = [ "org.final" ] } ];
    }

let test_role_additions_target () =
  expect_validation_error "role addition unknown" "role.additions.target"
    {
      (one_rule base_rule) with
      roles = [ { base_role with additions = [ "missing.rule" ] } ];
    }

let test_role_duplicate () =
  expect_validation_error "duplicate role" "role.duplicate"
    {
      (one_rule base_rule) with
      roles = [ base_role; { base_role with source = "roles/other.yaml" } ];
    }

let test_adapter_id () =
  expect_validation_error "adapter id" "adapter.id"
    { (one_rule base_rule) with adapters = [ { base_adapter with id = "vim" } ] }

let test_adapter_version () =
  expect_validation_error "adapter version" "adapter.version"
    { (one_rule base_rule) with adapters = [ { base_adapter with version = 0 } ] }

let test_adapter_control () =
  expect_validation_error "adapter control" "adapter.control"
    {
      (one_rule base_rule) with
      adapters =
        [ { base_adapter with controls = [ ("advisory", [ "ci-review" ]) ] } ];
    }

let test_adapter_unsupported () =
  expect_validation_error "adapter unsupported" "adapter.unsupported"
    {
      (one_rule base_rule) with
      adapters = [ { base_adapter with unsupported = [] } ];
    }

let test_adapter_duplicate () =
  expect_validation_error "duplicate adapter" "adapter.duplicate-id"
    {
      (one_rule base_rule) with
      adapters =
        [ base_adapter; { base_adapter with source = "adapters/other.yaml" } ];
    }

let test_exception_id () =
  expect_validation_error "exception id" "exception.id"
    {
      (one_rule base_rule) with
      exceptions = [ { base_exception with id = "fixture" } ];
    }

let test_exception_rule () =
  expect_validation_error "exception rule" "exception.rule"
    {
      (one_rule base_rule) with
      exceptions = [ { base_exception with rule = "missing.rule" } ];
    }

let test_exception_scope () =
  expect_validation_error "exception scope" "exception.scope"
    {
      (one_rule base_rule) with
      exceptions = [ { base_exception with scope = "repo:*" } ];
    }

let test_exception_reason () =
  expect_validation_error "exception reason" "exception.reason"
    {
      (one_rule base_rule) with
      exceptions = [ { base_exception with reason = "" } ];
    }

let test_exception_approver () =
  expect_validation_error "exception approver" "exception.approver"
    {
      (one_rule base_rule) with
      exceptions = [ { base_exception with approver = "TBD" } ];
    }

let test_exception_expires_date () =
  expect_validation_error "exception calendar date" "exception.expires"
    {
      (one_rule base_rule) with
      exceptions = [ { base_exception with expires = "2026-02-30" } ];
    }

let test_exception_duplicate () =
  expect_validation_error "duplicate exception" "exception.duplicate-id"
    {
      (one_rule base_rule) with
      exceptions =
        [ base_exception; { base_exception with line = 2 } ];
    }

let test_skill_name () =
  expect_validation_error "skill name" "skill.name"
    { (one_rule base_rule) with skills = [ { base_skill with name = "wrong" } ] }

let test_skill_description () =
  expect_validation_error "skill description" "skill.description"
    {
      (one_rule base_rule) with
      skills = [ { base_skill with description = "" } ];
    }

let test_skill_triggers () =
  expect_validation_error "skill triggers" "skill.triggers"
    { (one_rule base_rule) with skills = [ { base_skill with triggers = [] } ] }

let test_skill_prerequisites () =
  expect_validation_error "skill prerequisites" "skill.prerequisites"
    {
      (one_rule base_rule) with
      skills = [ { base_skill with prerequisites = [] } ];
    }

let test_skill_stop_conditions () =
  expect_validation_error "skill stop conditions" "skill.stop-conditions"
    {
      (one_rule base_rule) with
      skills = [ { base_skill with stop_conditions = [] } ];
    }

let test_skill_verification () =
  expect_validation_error "skill verification" "skill.verification"
    { (one_rule base_rule) with skills = [ { base_skill with verification = [] } ] }

let test_skill_rules_empty () =
  expect_validation_error "skill rules empty" "skill.rules.empty"
    { (one_rule base_rule) with skills = [ { base_skill with rules = [] } ] }

let test_skill_rules_target () =
  expect_validation_error "skill rule target" "skill.rules.target"
    {
      (one_rule base_rule) with
      skills = [ { base_skill with rules = [ "missing.rule" ] } ];
    }

let test_skill_body () =
  expect_validation_error "skill body" "skill.body"
    { (one_rule base_rule) with skills = [ { base_skill with body = "  \n" } ] }

let test_skill_tests () =
  expect_validation_error "skill tests" "skill.tests"
    {
      (one_rule base_rule) with
      skills = [ { base_skill with files = [ ("scripts/x.sh", "") ] } ];
    }

let test_secret_private_key () =
  expect_validation_error "pem private key" "secret.private-key"
    (one_rule
       {
         base_rule with
         body = "-----BEGIN RSA PRIVATE KEY-----\nMIIB\n";
       })

let test_secret_credential_assignment () =
  expect_validation_error "credential assignment" "secret.credential-assignment"
    (one_rule { base_rule with body = "password=hunter2\n" })

let test_budget_core () =
  expect_validation_error "core budget" "budget.core"
    {
      (one_rule base_rule) with
      policy = { base_policy with core_max_bytes = 1 };
    }

let test_budget_rules () =
  let other = { base_rule with id = "test.other"; source = "rules/other.md" } in
  expect_validation_error "rule budget" "budget.rules"
    {
      (with_rules [ base_rule; other ]) with
      policy = { base_policy with max_rules = 1 };
    }

(* ------------------------------------------------------------------ *)
(* Runner                                                             *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia source"
    [
      ( "corpus",
        [
          Alcotest.test_case "real corpus loads and validates" `Quick
            test_real_corpus_loads;
          Alcotest.test_case "sha256 hex" `Quick test_sha256_hex;
          Alcotest.test_case "deterministic load" `Quick test_determinism;
          Alcotest.test_case "clean corpus has no secret diagnostics" `Quick
            test_clean_corpus_has_no_secret_diagnostics;
        ] );
      ( "invalid",
        [
          Alcotest.test_case "duplicate id" `Quick test_duplicate_id;
          Alcotest.test_case "missing owner" `Quick test_missing_owner;
          Alcotest.test_case "placeholder owner" `Quick test_placeholder_owner;
          Alcotest.test_case "unknown field" `Quick test_unknown_field;
          Alcotest.test_case "duplicate yaml key" `Quick test_duplicate_key;
          Alcotest.test_case "conflicting claims" `Quick test_conflicting_claims;
          Alcotest.test_case "broken skill reference" `Quick
            test_broken_skill_reference;
          Alcotest.test_case "unauthorized override" `Quick
            test_unauthorized_override;
          Alcotest.test_case "expired exception" `Quick test_expired_exception;
          Alcotest.test_case "bad adapter control" `Quick
            test_bad_adapter_control;
          Alcotest.test_case "absolute skill path" `Quick
            test_absolute_skill_file;
          Alcotest.test_case "secret in rule body" `Quick test_secret_rule;
          Alcotest.test_case "sorted diagnostics" `Quick
            test_sorted_diagnostics;
        ] );
      ( "valid",
        [
          Alcotest.test_case "minimal source" `Quick test_minimal_valid;
          Alcotest.test_case "validation is repeatable" `Quick
            test_validation_is_repeatable;
          Alcotest.test_case "secret false positives" `Quick
            test_secret_false_positives;
        ] );
      ( "validation",
        [
          Alcotest.test_case "base model is valid" `Quick test_base_model_valid;
          Alcotest.test_case "policy schema" `Quick test_policy_schema;
          Alcotest.test_case "policy release" `Quick test_policy_release;
          Alcotest.test_case "policy compiler" `Quick test_policy_compiler;
          Alcotest.test_case "policy maintainers" `Quick
            test_policy_maintainers_empty;
          Alcotest.test_case "policy maintainer role" `Quick
            test_policy_maintainer_role;
          Alcotest.test_case "policy budget" `Quick test_policy_budget;
          Alcotest.test_case "rule id" `Quick test_rule_id;
          Alcotest.test_case "rule revision" `Quick test_rule_revision;
          Alcotest.test_case "rule owner missing" `Quick
            test_rule_owner_missing;
          Alcotest.test_case "rule owner placeholder" `Quick
            test_rule_owner_placeholder;
          Alcotest.test_case "rule scope" `Quick test_rule_scope;
          Alcotest.test_case "rule action" `Quick test_rule_action_unknown;
          Alcotest.test_case "rule guard action" `Quick
            test_rule_guard_action;
          Alcotest.test_case "rule key/value" `Quick test_rule_key_value;
          Alcotest.test_case "rule supersedes syntax" `Quick
            test_rule_supersedes_syntax;
          Alcotest.test_case "rule supersedes self" `Quick
            test_rule_supersedes_self;
          Alcotest.test_case "rule supersedes existing" `Quick
            test_rule_supersedes_existing;
          Alcotest.test_case "rule supersedes valid" `Quick
            test_rule_supersedes_valid;
          Alcotest.test_case "rule enforcement control" `Quick
            test_rule_enforcement_control;
          Alcotest.test_case "final consent needs a control" `Quick
            test_rule_enforcement_controls_final;
          Alcotest.test_case "final rule owner" `Quick test_rule_final_owner;
          Alcotest.test_case "org rule relaxes" `Quick test_rule_relaxes_org;
          Alcotest.test_case "duplicate rule id" `Quick
            test_rule_duplicate_id;
          Alcotest.test_case "rule key conflict" `Quick
            test_rule_key_conflict;
          Alcotest.test_case "repo relaxes unknown" `Quick
            test_rule_relaxes_target;
          Alcotest.test_case "repo relaxes final" `Quick
            test_rule_relaxes_final;
          Alcotest.test_case "role name" `Quick test_role_name;
          Alcotest.test_case "role description" `Quick test_role_description;
          Alcotest.test_case "role charter" `Quick test_role_charter;
          Alcotest.test_case "role output contract" `Quick
            test_role_output_contract;
          Alcotest.test_case "role memory contract" `Quick
            test_role_memory_contract;
          Alcotest.test_case "role relaxes unknown" `Quick
            test_role_relaxes_target;
          Alcotest.test_case "role relaxes final" `Quick
            test_role_relaxes_final;
          Alcotest.test_case "role addition unknown" `Quick
            test_role_additions_target;
          Alcotest.test_case "duplicate role" `Quick test_role_duplicate;
          Alcotest.test_case "adapter id" `Quick test_adapter_id;
          Alcotest.test_case "adapter version" `Quick test_adapter_version;
          Alcotest.test_case "adapter control" `Quick test_adapter_control;
          Alcotest.test_case "adapter unsupported" `Quick
            test_adapter_unsupported;
          Alcotest.test_case "duplicate adapter" `Quick
            test_adapter_duplicate;
          Alcotest.test_case "exception id" `Quick test_exception_id;
          Alcotest.test_case "exception rule" `Quick test_exception_rule;
          Alcotest.test_case "exception scope" `Quick test_exception_scope;
          Alcotest.test_case "exception reason" `Quick test_exception_reason;
          Alcotest.test_case "exception approver" `Quick
            test_exception_approver;
          Alcotest.test_case "exception calendar date" `Quick
            test_exception_expires_date;
          Alcotest.test_case "duplicate exception" `Quick
            test_exception_duplicate;
          Alcotest.test_case "skill name" `Quick test_skill_name;
          Alcotest.test_case "skill description" `Quick
            test_skill_description;
          Alcotest.test_case "skill triggers" `Quick test_skill_triggers;
          Alcotest.test_case "skill prerequisites" `Quick
            test_skill_prerequisites;
          Alcotest.test_case "skill stop conditions" `Quick
            test_skill_stop_conditions;
          Alcotest.test_case "skill verification" `Quick
            test_skill_verification;
          Alcotest.test_case "skill rules empty" `Quick
            test_skill_rules_empty;
          Alcotest.test_case "skill rule target" `Quick
            test_skill_rules_target;
          Alcotest.test_case "skill body" `Quick test_skill_body;
          Alcotest.test_case "skill tests" `Quick test_skill_tests;
          Alcotest.test_case "pem private key" `Quick
            test_secret_private_key;
          Alcotest.test_case "credential assignment" `Quick
            test_secret_credential_assignment;
          Alcotest.test_case "core budget" `Quick test_budget_core;
          Alcotest.test_case "rule budget" `Quick test_budget_rules;
        ] );
      ( "fragments",
        [
          Alcotest.test_case "front matter body offset" `Quick
            test_parse_frontmatter_offset;
          Alcotest.test_case "absent front matter" `Quick
            test_parse_frontmatter_absent;
          Alcotest.test_case "unterminated front matter" `Quick
            test_parse_frontmatter_unterminated;
          Alcotest.test_case "prose fragment" `Quick test_load_fragment_prose;
          Alcotest.test_case "inline rules fragment" `Quick
            test_load_fragment_rules;
          Alcotest.test_case "fragment relaxes" `Quick
            test_load_fragment_relaxes;
        ] );
      ( "exceptions",
        [
          Alcotest.test_case "expiry boundary" `Quick test_exception_expiry;
          Alcotest.test_case "today from environment" `Quick
            test_today_environment;
        ] );
    ]
