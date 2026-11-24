(** A read-only Model Context Protocol adapter over one pinned release.

    The adapter serves the same immutable release that local rendering would
    produce. It selects exactly one release at startup and never changes it:
    a resource URI or tool call that names another release is rejected, so a
    new remote release cannot alter a run already pinned to one digest.

    Protocol transport is one JSON object per line on stdin and exactly one
    JSON-RPC 2.0 response per request on stdout. Diagnostics (including the
    explicit cache-fallback notice) go to stderr only. This module never
    writes rendered policy, opens a network connection, reads a credential,
    or consults a provider.

    Supported requests: [initialize], [notifications/initialized] (no
    response), [ping], [resources/list], [resources/read], [tools/list],
    [tools/call], [shutdown], and [exit]. Unknown methods return [-32601];
    malformed JSON returns [-32700]; a batch array returns [-32600]; invalid
    params, an unknown resource, or a release mismatch return [-32602]; an
    internal failure returns [-32603].

    Release selection:

    - [release = Some digest] loads only that cached release; a missing or
      incomplete cache entry is a startup error.
    - otherwise the source at [source] (or the discovered source) is loaded,
      composed for [repo]/[role]/[client], and rendered for that client. The
      manifest is built exactly as [agentctl render] builds it.
    - if and only if the source is unavailable, the newest cached digest is
      served (sorted ascending by digest string, last one first, with fallback
      across a corrupt entry) and the choice is reported on stderr. There is
      never an empty-policy substitution and never an invented release. Cached
      core, rule bodies, and skill bytes are reconstructed from the verified
      cached artifacts; a composed rule whose body those artifacts do not
      carry is omitted with a stderr diagnostic rather than invented.

    Startup failure exits with code 2 and a single clear stderr line. *)

val protocol_version : string
(** ["2024-11-05"]. *)

val server_name : string
(** ["agentctl-mcp"]. *)

val run :
  ?source:string ->
  ?repo:string ->
  ?role:string ->
  ?client:string ->
  ?project:string ->
  ?release:string ->
  unit ->
  int
(** [run ()] serves one pinned release over stdio and returns the process exit
    code. [release] is a cached release digest. [client] defaults to the lock
    client or [generic-agents], matching composition. *)
