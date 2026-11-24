(** In-memory model of one policy source tree.

    These types are plain data: no filesystem handles, absolute machine
    paths, or filesystem-order dependence. [Source.load] builds them and
    [Source.validate] checks the semantic invariants. *)

type diagnostic = {
  code : string;
  path : string;
  line : int option;
  message : string;
}

type strength = Must | Should | May

type kind = Instruction | Procedure | Knowledge | Capability

type class_ = Advisory | Gate | Consent | Prohibition

type enforcement = { cls : class_; controls : string list }

type guard = { action : string; paths : string list; hosts : string list }

type layer = Organization | Repository | Role

type rule = {
  id : string;
  revision : int;
  owner : string;
  strength : strength;
  scope : string list;
  final : bool;
  kind : kind;
  title : string option;
  enforcement : enforcement option;
  actions : string list;
  guards : guard list;
  key : string option;
  value : string option;
  relaxes : string list;
  supersedes : string option;
  body : string;
  source : string;
  line : int;
  layer : layer;
}

type exception_ = {
  id : string;
  rule : string;
  scope : string;
  reason : string;
  approver : string;
  expires : string;
  source : string;
  line : int;
}

type skill = {
  name : string;
  description : string;
  triggers : string list;
  prerequisites : string list;
  stop_conditions : string list;
  verification : string list;
  rules : string list;
  body : string;
  files : (string * string) list;
      (** [(relative path, sha256 hex)] sorted by path, relative to the skill
          directory. [SKILL.md] and [scripts/...] form the installable unit. *)
  source : string;
}

type role = {
  name : string;
  description : string;
  charter : string;
  output_contract : string;
  memory_contract : string;
  relaxes : string list;
  additions : string list;
  source : string;
}

type instructions = { file : string; discovery : string list; includes : bool }

type skill_config = {
  dir : string;
  auto_discovery : bool;
  explicit_reference : bool;
}

type adapter = {
  id : string;
  version : int;
  description : string;
  instructions : instructions;
  skills : skill_config;
  roles : bool;
  mcp : bool;
  hooks : bool;
  permissions : bool;
  atomic_output : bool;
  controls : (string * string list) list;
  unsupported : string list;
  source : string;
}

type policy = {
  schema : string;
  release : string;
  compiler : string;
  description : string option;
  maintainers : (string * string) list;
  core_max_bytes : int;
  max_rules : int;
  source : string;
}

type source = {
  root : string;
  policy : policy;
  rules : rule list;
  skills : skill list;
  roles : role list;
  adapters : adapter list;
  exceptions : exception_ list;
  files : (string * string) list;
      (** [(relative path, sha256 hex)] for every file the loader read,
          sorted by path, relative to [root]. *)
}
