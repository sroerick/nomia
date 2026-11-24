(* Strict recursive-descent parser for the nomia YAML subset.
   See yaml_lite.mli for the accepted and rejected forms. *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | List of t list
  | Map of (string * t) list

type block_style = Literal | Folded

type value_kind =
  | No_value
  | Block of block_style
  | Inline of string

exception Parse_error of int * string

let error line fmt =
  Printf.ksprintf (fun msg -> raise (Parse_error (line, msg))) fmt

(* ------------------------------------------------------------------ *)
(* Small string helpers                                               *)
(* ------------------------------------------------------------------ *)

let is_ws = function
  | ' ' | '\t' -> true
  | _ -> false

let trim_left s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_ws s.[!i] do
    incr i
  done;
  if !i = 0 then s else String.sub s !i (len - !i)

let trim_right s =
  let len = String.length s in
  let i = ref len in
  while !i > 0 && is_ws s.[!i - 1] do
    decr i
  done;
  if !i = len then s else String.sub s 0 !i

let contains haystack needle =
  let lh = String.length haystack in
  let ln = String.length needle in
  if ln = 0 then true
  else
    let rec loop i =
      if i + ln > lh then false
      else if String.sub haystack i ln = needle then true
      else loop (i + 1)
    in
    loop 0

let is_digit c = c >= '0' && c <= '9'

let is_int_literal s =
  let len = String.length s in
  if len = 0 then false
  else begin
    let i = ref 0 in
    if s.[0] = '+' || s.[0] = '-' then incr i;
    if !i >= len then false
    else begin
      let ok = ref true in
      while !i < len do
        if not (is_digit s.[!i]) then ok := false;
        incr i
      done;
      !ok
    end
  end

let is_float_literal s =
  let len = String.length s in
  if len = 0 then false
  else begin
    let i = ref 0 in
    if s.[0] = '+' || s.[0] = '-' then incr i;
    let before = ref 0 in
    while !i < len && is_digit s.[!i] do
      incr before;
      incr i
    done;
    let after = ref 0 in
    let has_dot = ref false in
    if !i < len && s.[!i] = '.' then begin
      has_dot := true;
      incr i;
      while !i < len && is_digit s.[!i] do
        incr after;
        incr i
      done
    end;
    let ok = ref true in
    let has_exp = ref false in
    if !i < len && (s.[!i] = 'e' || s.[!i] = 'E') then begin
      has_exp := true;
      incr i;
      if !i < len && (s.[!i] = '+' || s.[!i] = '-') then incr i;
      let exp_digits = ref 0 in
      while !i < len && is_digit s.[!i] do
        incr exp_digits;
        incr i
      done;
      if !exp_digits = 0 then ok := false
    end;
    let has_digits = !before > 0 || !after > 0 in
    !ok && has_digits && (!has_dot || !has_exp) && !i = len
  end

(* ------------------------------------------------------------------ *)
(* Lines and indentation                                              *)
(* ------------------------------------------------------------------ *)

type line = {
  num : int;
  indent : int;
  content : string;
}

let is_blank l = String.trim l.content = ""

let is_comment l =
  String.length l.content > 0 && l.content.[0] = '#'

let is_trivia l = is_blank l || is_comment l

let is_seq_item s =
  String.length s >= 1
  && s.[0] = '-'
  && (String.length s = 1 || s.[1] = ' ' || s.[1] = '\t')

let is_doc_marker s =
  let s = trim_right s in
  (String.starts_with ~prefix:"---" s
  && (String.length s = 3 || s.[3] = ' '))
  || (String.starts_with ~prefix:"..." s
     && (String.length s = 3 || s.[3] = ' '))

let preprocess text =
  let raw_lines = String.split_on_char '\n' text in
  Array.of_list
    (List.mapi
       (fun i raw ->
         let len = String.length raw in
         let raw =
           if len > 0 && raw.[len - 1] = '\r' then String.sub raw 0 (len - 1)
           else raw
         in
         let len = String.length raw in
         let j = ref 0 in
         while !j < len && (raw.[!j] = ' ' || raw.[!j] = '\t') do
           if raw.[!j] = '\t' then
             error (i + 1)
               "tab character used for indentation; YAML indentation must use \
                spaces";
           incr j
         done;
         let indent = !j in
         {
           num = i + 1;
           indent;
           content = String.sub raw indent (len - indent);
         })
       raw_lines)

let skip_trivia lines idx =
  let n = Array.length lines in
  while !idx < n && is_trivia lines.(!idx) do
    incr idx
  done

let check_structural_marker l =
  if String.length l.content > 0 && l.content.[0] = '%' then
    error l.num "YAML directives ('%%...') are not supported";
  if is_doc_marker l.content then
    error l.num "multiple YAML documents are not supported (found '%s')"
      (String.trim l.content)

(* ------------------------------------------------------------------ *)
(* Quoted scalars                                                     *)
(* ------------------------------------------------------------------ *)

let hex_value line c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> error line "invalid hexadecimal digit '%c' in escape sequence" c

let read_hex line s pos count =
  if pos + count > String.length s then
    error line "truncated \\x/\\u/\\U escape sequence in double-quoted scalar";
  let v = ref 0 in
  for k = 0 to count - 1 do
    v := (!v lsl 4) lor hex_value line s.[pos + k]
  done;
  !v

let add_utf8 line b cp =
  if cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then
    error line "invalid Unicode code point U+%X in escape sequence" cp;
  if cp <= 0x7F then Buffer.add_char b (Char.chr cp)
  else if cp <= 0x7FF then begin
    Buffer.add_char b (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else if cp <= 0xFFFF then begin
    Buffer.add_char b (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else begin
    Buffer.add_char b (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char b (Char.chr (0x80 lor (cp land 0x3F)))
  end

let parse_single_quoted s start line =
  let len = String.length s in
  let b = Buffer.create (len - start) in
  let i = ref (start + 1) in
  let closed = ref false in
  while not !closed do
    if !i >= len then error line "unterminated single-quoted scalar";
    if s.[!i] = '\'' then
      if !i + 1 < len && s.[!i + 1] = '\'' then begin
        Buffer.add_char b '\'';
        i := !i + 2
      end
      else begin
        closed := true;
        incr i
      end
    else begin
      Buffer.add_char b s.[!i];
      incr i
    end
  done;
  (Buffer.contents b, !i)

let parse_double_quoted s start line =
  let len = String.length s in
  let b = Buffer.create (len - start) in
  let i = ref (start + 1) in
  let closed = ref false in
  while not !closed do
    if !i >= len then error line "unterminated double-quoted scalar";
    match s.[!i] with
    | '"' ->
        closed := true;
        incr i
    | '\\' ->
        if !i + 1 >= len then
          error line "unterminated escape sequence in double-quoted scalar";
        (match s.[!i + 1] with
        | '0' ->
            Buffer.add_char b '\000';
            i := !i + 2
        | 'a' ->
            Buffer.add_char b '\007';
            i := !i + 2
        | 'b' ->
            Buffer.add_char b '\b';
            i := !i + 2
        | 't' ->
            Buffer.add_char b '\t';
            i := !i + 2
        | 'n' ->
            Buffer.add_char b '\n';
            i := !i + 2
        | 'v' ->
            Buffer.add_char b '\011';
            i := !i + 2
        | 'f' ->
            Buffer.add_char b '\012';
            i := !i + 2
        | 'r' ->
            Buffer.add_char b '\r';
            i := !i + 2
        | 'e' ->
            Buffer.add_char b '\027';
            i := !i + 2
        | '"' ->
            Buffer.add_char b '"';
            i := !i + 2
        | '\\' ->
            Buffer.add_char b '\\';
            i := !i + 2
        | '/' ->
            Buffer.add_char b '/';
            i := !i + 2
        | ' ' ->
            Buffer.add_char b ' ';
            i := !i + 2
        | 'N' ->
            add_utf8 line b 0x85;
            i := !i + 2
        | '_' ->
            add_utf8 line b 0xA0;
            i := !i + 2
        | 'L' ->
            add_utf8 line b 0x2028;
            i := !i + 2
        | 'P' ->
            add_utf8 line b 0x2029;
            i := !i + 2
        | 'x' ->
            let v = read_hex line s (!i + 2) 2 in
            add_utf8 line b v;
            i := !i + 4
        | 'u' ->
            let v = read_hex line s (!i + 2) 4 in
            add_utf8 line b v;
            i := !i + 6
        | 'U' ->
            let v = read_hex line s (!i + 2) 8 in
            add_utf8 line b v;
            i := !i + 10
        | c ->
            error line "unsupported escape sequence '\\%c' in double-quoted \
                        scalar" c)
    | c ->
        Buffer.add_char b c;
        incr i
  done;
  (Buffer.contents b, !i)

let parse_quoted ~line s start =
  match s.[start] with
  | '\'' -> parse_single_quoted s start line
  | '"' -> parse_double_quoted s start line
  | c -> error line "internal error: expected a quote, found '%c'" c

let check_trailing ~line ~what s from =
  let len = String.length s in
  let i = ref from in
  while !i < len && is_ws s.[!i] do
    incr i
  done;
  if !i >= len then ()
  else if s.[!i] = '#' && !i > from then ()
  else
    error line "unexpected content after %s: '%s'" what
      (String.sub s !i (len - !i))

(* ------------------------------------------------------------------ *)
(* Scalars                                                            *)
(* ------------------------------------------------------------------ *)

let classify_plain ~line s =
  match s with
  | "~" | "null" | "Null" | "NULL" -> Null
  | "true" | "True" | "TRUE" -> Bool true
  | "false" | "False" | "FALSE" -> Bool false
  | _ ->
      if is_int_literal s then (
        match int_of_string_opt s with
        | Some i -> Int i
        | None -> error line "integer literal out of range: %s" s)
      else if is_float_literal s then (
        match float_of_string_opt s with
        | Some f -> Float f
        | None -> error line "invalid floating-point literal: %s" s)
      else String s

(* A '#' starts a comment at the beginning of a scalar or after whitespace.
   A '#' glued to preceding text is part of the scalar. *)
let strip_comment s =
  let len = String.length s in
  let rec loop i =
    if i >= len then s
    else if s.[i] = '#' && (i = 0 || is_ws s.[i - 1]) then String.sub s 0 i
    else loop (i + 1)
  in
  loop 0

let validate_plain_key line key =
  if key = "" then error line "empty mapping key";
  match key.[0] with
  | '&' -> error line "anchors ('&...') are not supported as mapping keys"
  | '*' -> error line "aliases ('*...') are not supported as mapping keys"
  | '!' -> error line "tags ('!...') are not supported as mapping keys"
  | '[' | ']' | '{' | '}' | ',' | '?' ->
      error line "flow or explicit mapping key syntax is not supported"
  | '%' -> error line "YAML directives are not supported"
  | '@' | '`' ->
      error line "reserved indicator '%c' is not supported as a mapping key"
        key.[0]
  | '|' | '>' ->
      error line "a block scalar indicator cannot be used as a mapping key"
  | _ -> ()

(* Returns [Some (key, text_after_colon)] when this line starts a block
   mapping entry, [None] otherwise. Flow collection lines are never mapping
   entries. *)
let map_entry_opt ~line s =
  let len = String.length s in
  if len = 0 then None
  else if s.[0] = '[' || s.[0] = '{' then None
  else if s.[0] = '"' || s.[0] = '\'' then begin
    let key, next = parse_quoted ~line s 0 in
    if key = "" then error line "empty mapping key";
    let j = ref next in
    while !j < len && s.[!j] = ' ' do
      incr j
    done;
    if !j < len && s.[!j] = ':' then
      Some (key, String.sub s (!j + 1) (len - !j - 1))
    else None
  end
  else begin
    let i = ref 0 in
    let found = ref (-1) in
    let stop = ref false in
    while !i < len && not !stop do
      let c = s.[!i] in
      if c = '#' && (!i = 0 || is_ws s.[!i - 1]) then stop := true
      else if c = ':' && (!i + 1 = len || is_ws s.[!i + 1]) then begin
        found := !i;
        stop := true
      end
      else incr i
    done;
    if !found < 0 then None
    else begin
      let key = String.sub s 0 !found |> trim_right in
      validate_plain_key line key;
      Some (key, String.sub s (!found + 1) (len - !found - 1))
    end
  end

let rec parse_inline ~line s =
  let s = trim_left s in
  if s = "" then Null
  else
    match s.[0] with
    | '#' -> Null
    | '&' -> error line "anchors ('&...') are not supported"
    | '*' -> error line "aliases ('*...') are not supported"
    | '!' -> error line "tags ('!...') are not supported"
    | '{' -> error line "flow mappings ('{...}') are not supported"
    | '|' | '>' ->
        error line
          "block scalar indicators are only valid after a mapping key or \
           sequence dash"
    | '%' -> error line "YAML directives are not supported"
    | '@' | '`' ->
        error line "reserved indicator '%c' is not supported" s.[0]
    | '?' -> error line "explicit mapping keys ('? ...') are not supported"
    | ']' | '}' | ',' ->
        error line "unexpected flow indicator '%c' in a block context" s.[0]
    | '[' -> parse_flow_seq ~line s
    | '\'' | '"' ->
        let v, next = parse_quoted ~line s 0 in
        check_trailing ~line ~what:"quoted scalar" s next;
        String v
    | _ ->
        let plain = trim_right (strip_comment s) in
        if plain = "" then Null
        else begin
          if contains plain ": " then
            error line "plain scalar may not contain ': '; quote the value";
          if String.starts_with ~prefix:"- " plain then
            error line
              "a block sequence entry cannot start on the same line as a \
               value; put '-' on its own line or quote the value";
          classify_plain ~line plain
        end

and parse_flow_seq ~line s =
  let len = String.length s in
  let i = ref 1 in
  let skip_spaces () =
    while !i < len && is_ws s.[!i] do
      incr i
    done
  in
  let parse_element () =
    skip_spaces ();
    if !i >= len then error line "unterminated flow sequence (missing ']')";
    match s.[!i] with
    | '[' -> error line "nested flow sequences are not supported"
    | '{' -> error line "flow mappings ('{...}') are not supported"
    | ']' | ',' -> error line "empty element in flow sequence"
    | '&' -> error line "anchors ('&...') are not supported"
    | '*' -> error line "aliases ('*...') are not supported"
    | '!' -> error line "tags ('!...') are not supported"
    | '|' | '>' ->
        error line "block scalar indicators are not valid in a flow sequence"
    | '%' -> error line "YAML directives are not supported"
    | '@' | '`' ->
        error line "reserved indicator '%c' is not supported" s.[!i]
    | '?' ->
        error line "explicit mapping keys ('? ...') are not supported"
    | '\'' | '"' ->
        let v, next = parse_quoted ~line s !i in
        i := next;
        String v
    | _ ->
        let raw_start = !i in
        while !i < len && s.[!i] <> ',' && s.[!i] <> ']' do
          if
            s.[!i] = '#'
            && !i > raw_start
            && is_ws s.[!i - 1]
          then
            error line
              "comments are not supported inside a single-line flow sequence";
          incr i
        done;
        let raw = String.sub s raw_start (!i - raw_start) |> trim_right in
        if raw = "" then error line "empty element in flow sequence";
        String.iter
          (fun c ->
            if c = '[' || c = '{' || c = '}' then
              error line "unexpected flow indicator '%c' in plain flow scalar" c)
          raw;
        if contains raw ": " then
          error line "flow mappings are not supported (plain scalar contains \
                      ': ')";
        if String.starts_with ~prefix:"- " raw then
          error line
            "a flow scalar may not start with '- '; quote the value";
        classify_plain ~line raw
  in
  skip_spaces ();
  let items =
    if !i < len && s.[!i] = ']' then begin
      incr i;
      []
    end
    else begin
      let acc = ref [] in
      let continue = ref true in
      while !continue do
        let item = parse_element () in
        acc := item :: !acc;
        skip_spaces ();
        if !i >= len then error line "unterminated flow sequence (missing ']')"
        else
          match s.[!i] with
          | ',' ->
              incr i;
              skip_spaces ();
              if !i < len && s.[!i] = ']' then begin
                incr i;
                continue := false
              end
          | ']' ->
              incr i;
              continue := false
          | c -> error line "expected ',' or ']' in flow sequence, found '%c'" c
      done;
      List.rev !acc
    end
  in
  check_trailing ~line ~what:"flow sequence" s !i;
  List items

(* ------------------------------------------------------------------ *)
(* Block strings                                                      *)
(* ------------------------------------------------------------------ *)

let classify_value ~line rest =
  let t = trim_left rest in
  if t = "" || t.[0] = '#' then No_value
  else if t.[0] = '\'' || t.[0] = '"' || t.[0] = '[' || t.[0] = '{' then
    Inline rest
  else
    let core = trim_right (strip_comment t) in
    if core = "" then No_value
    else if core = "|" then Block Literal
    else if core = ">" then Block Folded
    else if core.[0] = '|' || core.[0] = '>' then
      error line "unsupported block scalar header '%s' (only '|' and '>' are \
                  supported)" core
    else Inline rest

let fold_lines lines =
  let b = Buffer.create 64 in
  let pending_blank = ref 0 in
  let first = ref true in
  List.iter
    (fun line ->
      if line = "" then incr pending_blank
      else begin
        if not !first then
          if !pending_blank = 0 then Buffer.add_char b ' '
          else Buffer.add_string b (String.make !pending_blank '\n');
        Buffer.add_string b line;
        first := false;
        pending_blank := 0
      end)
    lines;
  Buffer.contents b

let trim_trailing_empty ls =
  let rec drop = function
    | "" :: rest -> drop rest
    | xs -> xs
  in
  List.rev (drop (List.rev ls))

let parse_block_scalar lines idx style parent_indent =
  let n = Array.length lines in
  let i = ref !idx in
  let block_indent = ref None in
  let seen = ref [] in
  let stopped = ref false in
  while (not !stopped) && !i < n do
    let l = lines.(!i) in
    if is_blank l then begin
      seen := "" :: !seen;
      incr i
    end
    else if l.indent > parent_indent then begin
      let bi =
        match !block_indent with
        | None ->
            block_indent := Some l.indent;
            l.indent
        | Some bi ->
            if l.indent < bi then
              error l.num
                "inconsistent indentation in block scalar: expected at least %d \
                 spaces, found %d" bi l.indent
            else bi
      in
      seen := (String.make (l.indent - bi) ' ' ^ l.content) :: !seen;
      incr i
    end
    else stopped := true
  done;
  idx := !i;
  let ls = trim_trailing_empty (List.rev !seen) in
  let body =
    match style with
    | Literal -> String.concat "\n" ls
    | Folded -> fold_lines ls
  in
  if ls = [] then String "" else String (body ^ "\n")

(* ------------------------------------------------------------------ *)
(* Block structure                                                    *)
(* ------------------------------------------------------------------ *)

let rec parse_block lines idx indent =
  skip_trivia lines idx;
  let n = Array.length lines in
  if !idx >= n then error 1 "expected a mapping or sequence but input ended";
  let l = lines.(!idx) in
  if l.indent <> indent then
    error l.num "inconsistent indentation: expected %d spaces, found %d" indent
      l.indent;
  check_structural_marker l;
  if is_seq_item l.content then parse_seq lines idx indent
  else if map_entry_opt ~line:l.num l.content <> None then
    parse_map lines idx indent
  else error l.num "expected a mapping or sequence at this indentation"

and parse_map lines idx indent =
  let n = Array.length lines in
  let rec loop acc =
    skip_trivia lines idx;
    if !idx >= n then Map (List.rev acc)
    else
      let l = lines.(!idx) in
      if l.indent < indent then Map (List.rev acc)
      else if l.indent > indent then
        error l.num "unexpected indentation: expected %d spaces, found %d"
          indent l.indent
      else begin
        check_structural_marker l;
        if is_seq_item l.content then
          error l.num "a sequence item ('- ...') is not allowed inside a mapping";
        match map_entry_opt ~line:l.num l.content with
        | None -> error l.num "expected a 'key: value' mapping entry"
        | Some (key, rest) ->
            if List.mem_assoc key acc then
              error l.num "duplicate mapping key '%s'" key;
            incr idx;
            let value =
              match classify_value ~line:l.num rest with
              | No_value ->
                  skip_trivia lines idx;
                  if !idx < n && lines.(!idx).indent > indent then
                    parse_block lines idx lines.(!idx).indent
                  else Null
              | Block style -> parse_block_scalar lines idx style indent
              | Inline text -> parse_inline ~line:l.num text
            in
            loop ((key, value) :: acc)
      end
  in
  loop []

and parse_seq lines idx indent =
  let n = Array.length lines in
  let rec loop acc =
    skip_trivia lines idx;
    if !idx >= n then List (List.rev acc)
    else
      let l = lines.(!idx) in
      if l.indent < indent then List (List.rev acc)
      else if l.indent > indent then
        error l.num "unexpected indentation: expected %d spaces, found %d"
          indent l.indent
      else begin
        check_structural_marker l;
        if not (is_seq_item l.content) then
          error l.num "expected a sequence item ('- ...') at this indentation";
        let content = l.content in
        let len = String.length content in
        let j = ref 1 in
        while !j < len && content.[!j] = ' ' do
          incr j
        done;
        if !j < len && content.[!j] = '\t' then
          error l.num "tab character after '-' is not supported; use spaces";
        let child_indent = indent + !j in
        let rest =
          if !j >= len then "" else String.sub content !j (len - !j)
        in
        if rest = "" || rest.[0] = '#' then begin
          incr idx;
          skip_trivia lines idx;
          let item =
            if !idx < n && lines.(!idx).indent > indent then
              parse_block lines idx lines.(!idx).indent
            else Null
          in
          loop (item :: acc)
        end
        else if
          is_seq_item rest || map_entry_opt ~line:l.num rest <> None
        then begin
          lines.(!idx) <- { l with indent = child_indent; content = rest };
          let item = parse_block lines idx child_indent in
          loop (item :: acc)
        end
        else
          match classify_value ~line:l.num rest with
          | No_value -> error l.num "internal error: empty sequence item"
          | Block style ->
              incr idx;
              let item = parse_block_scalar lines idx style indent in
              loop (item :: acc)
          | Inline text ->
              incr idx;
              let item = parse_inline ~line:l.num text in
              loop (item :: acc)
      end
  in
  loop []

(* ------------------------------------------------------------------ *)
(* Entry points and accessors                                         *)
(* ------------------------------------------------------------------ *)

let parse_string ?(source = "<string>") text =
  try
    let lines = preprocess text in
    let idx = ref 0 in
    let n = Array.length lines in
    skip_trivia lines idx;
    if !idx < n then begin
      let l = lines.(!idx) in
      if l.indent = 0 && String.trim l.content = "---" then begin
        incr idx;
        skip_trivia lines idx
      end
    end;
    let value =
      if !idx >= n then Null
      else begin
        let l = lines.(!idx) in
        check_structural_marker l;
        if l.indent <> 0 then
          error l.num
            "top-level content must start at column 0 (found %d leading spaces)"
            l.indent;
        if is_seq_item l.content || map_entry_opt ~line:l.num l.content <> None
        then parse_block lines idx 0
        else begin
          incr idx;
          let v = parse_inline ~line:l.num l.content in
          skip_trivia lines idx;
          if !idx < n then begin
            let extra = lines.(!idx) in
            check_structural_marker extra;
            error extra.num
              "unexpected content after the top-level scalar (only one document \
               is supported)"
          end;
          v
        end
      end
    in
    skip_trivia lines idx;
    if !idx < n then begin
      let extra = lines.(!idx) in
      check_structural_marker extra;
      error extra.num
        "unexpected content after the document (check indentation; only one \
         document is supported)"
    end;
    Ok value
  with Parse_error (line, msg) ->
    Error (Printf.sprintf "%s:%d: %s" source line msg)

let parse_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> parse_string ~source:path text
  | exception Sys_error msg ->
      Error (Printf.sprintf "%s:0: cannot read file: %s" path msg)

let get key = function
  | Map entries -> List.assoc_opt key entries
  | Null | Bool _ | Int _ | Float _ | String _ | List _ -> None

let type_name = function
  | Null -> "null"
  | Bool _ -> "boolean"
  | Int _ -> "integer"
  | Float _ -> "float"
  | String _ -> "string"
  | List _ -> "list"
  | Map _ -> "mapping"

let get_string key t =
  match get key t with
  | None -> Error (Printf.sprintf "missing required key '%s'" key)
  | Some (String s) -> Ok s
  | Some v ->
      Error
        (Printf.sprintf "key '%s' must be a string, found %s" key
           (type_name v))

let get_list key t =
  match get key t with
  | None -> Error (Printf.sprintf "missing required key '%s'" key)
  | Some (List l) -> Ok l
  | Some v ->
      Error
        (Printf.sprintf "key '%s' must be a list, found %s" key (type_name v))

let get_bool key t =
  match get key t with
  | None -> Error (Printf.sprintf "missing required key '%s'" key)
  | Some (Bool b) -> Ok b
  | Some v ->
      Error
        (Printf.sprintf "key '%s' must be a boolean, found %s" key
           (type_name v))

let get_int key t =
  match get key t with
  | None -> Error (Printf.sprintf "missing required key '%s'" key)
  | Some (Int i) -> Ok i
  | Some v ->
      Error
        (Printf.sprintf "key '%s' must be an integer, found %s" key
           (type_name v))
