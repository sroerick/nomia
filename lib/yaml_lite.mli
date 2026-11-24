(** A strict, dependency-free parser for the small YAML subset used by
    nomia source files.

    The supported subset is deliberately narrow:

    - block mappings ({[key: value]}, {[key:]} followed by an indented block,
      arbitrary nesting);
    - block sequences ({[- item]}, {[- key: value]}, nested blocks);
    - flow sequences of scalars ({[[a, b]]}, {[[]]});
    - plain, single-quoted, and double-quoted scalars (double quotes support
      the standard YAML escapes);
    - integers, floats, [true]/[false] (and the usual case variants), and
      [null]/[~];
    - literal ([|]) and folded ([>]) block strings, with indentation stripped
      from every content line;
    - comments introduced by [#] at the start of a line or after whitespace;
    - blank lines.

    Everything else is a hard error rather than being silently ignored: tabs
    used for indentation, anchors, aliases, tags, directives, document markers,
    duplicate mapping keys, inconsistent indentation, unterminated quotes or
    flow sequences, flow mappings, nested flow sequences, explicit keys, and
    block scalar headers other than a bare [|] or [>].

    Block string newline behavior: the indentation of the first non-blank
    content line is stripped from all lines; trailing blank lines are dropped;
    the result ends with exactly one newline unless the block is empty. Folded
    blocks join adjacent content lines with a single space; each blank line
    becomes one newline.

    Quoted scalars must be complete on one line. Flow sequences must be
    complete on one line. A single optional leading [---] is accepted; any
    second document marker is an error.

    Errors are one line of the form ["<source>:<line>: <message>"]. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Map of (string * t) list

val parse_string : ?source:string -> string -> (t, string) result
(** [parse_string ~source text] parses one YAML document. [source] defaults to
    ["<string>"] and appears in error messages. Empty input and input
    containing only comments parse as [Null]. *)

val parse_file : string -> (t, string) result
(** [parse_file path] reads and parses [path], using the path as the source
    name in parse errors. *)

val get : string -> t -> t option
(** [get key value] looks up [key] in a mapping. It returns [None] for a
    non-mapping value or a missing key. *)

val get_string : string -> t -> (string, string) result
val get_list : string -> t -> (t list, string) result
val get_bool : string -> t -> (bool, string) result
val get_int : string -> t -> (int, string) result
(** Typed accessors. Each returns an actionable error naming the key and the
    type that was actually present. *)
