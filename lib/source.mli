(** Deterministic loading and validation of one policy source tree.

    [load] owns filesystem reads and the schema-shape checks that need raw
    YAML (unknown fields, value types, enum spellings). [validate] checks the
    semantic invariants. [load_and_validate] composes both.

    Diagnostics always carry source paths relative to [root]; private
    absolute machine paths never appear. Diagnostics are returned sorted by
    path, then line, then code. *)

val load : root:string -> (Model.source, Model.diagnostic list) result
(** Read [root] deterministically: files in sorted order, one rule per
    [rules/*.md] file, roles and adapters from [*.yaml], exceptions from
    [exceptions.yaml], every skill file under [skills/<name>/], and every
    reviewed fragment under [fragments/*.md]. Every read file is recorded in
    [source.files] with its sha256; fragments are hashed, not parsed. *)

val validate :
  ?today:string -> Model.source -> (unit, Model.diagnostic list) result
(** Check ownership, cross-references, precedence, exceptions, budgets,
    secrets, and installed-file paths. [today] defaults to
    [NOMIA_TODAY] when set, otherwise the UTC date. *)

val load_and_validate :
  root:string -> ?today:string -> (Model.source, Model.diagnostic list) result
(** [load] then [validate]. Because [?today] is the final argument, a caller
    that wants the environment/UTC default passes [?today:None]; callers that
    already hold a source can use [load] and then [validate]. *)

val sha256_hex : string -> string
(** Lowercase hex sha256 of the string. *)

val secret_findings : string -> string list
(** Conservative credential shapes: PEM private-key headers, AWS access-key
    shapes, and non-placeholder credential assignments. *)

val has_private_absolute_path : string -> bool
(** True for an absolute path that looks like a private machine/home path. *)

val parse_frontmatter :
  path:string -> string -> (Yaml_lite.t * string * int, Model.diagnostic list) result
(** Split Markdown into the YAML front-matter tree, the body, and the 1-based
    line number of the first body line. A file without an opening [---] is
    prose only: the tree is [Null], the body is the whole input, and the
    offset is 1. *)

val load_fragment :
  path:string ->
  (Model.rule list * string * (string * string) list, Model.diagnostic list) result
(** Read a repository fragment: Markdown with optional front matter. Front
    matter may carry a [rules:] list of inline rule mappings (decoded at the
    [Repository] layer) and a top-level [relaxes:] list, which is attached to
    each inline rule. Returns the rules, the body, and [(path, sha256)] for
    the fragment file. A fragment without front matter is prose only. *)

val load_fragment_rel :
  root:string ->
  rel:string ->
  (Model.rule list * string * (string * string) list, Model.diagnostic list) result
(** [load_fragment_rel ~root ~rel] reads [root/rel] and returns the same tuple
    as {!load_fragment}, with every diagnostic path and the recorded fragment
    path equal to [rel]. [rel] must be relative to [root]; an absolute path is
    a diagnostic rather than a host-path leak. *)
