open Nomia

module Y = Nomia.Yaml_lite

(* ------------------------------------------------------------------ *)
(* Helpers                                                            *)
(* ------------------------------------------------------------------ *)

let rec pp_value fmt = function
  | Y.Null -> Format.pp_print_string fmt "null"
  | Y.Bool b -> Format.fprintf fmt "%b" b
  | Y.Int i -> Format.fprintf fmt "%d" i
  | Y.Float f -> Format.fprintf fmt "%g" f
  | Y.String s -> Format.fprintf fmt "%S" s
  | Y.List items ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
           pp_value)
        items
  | Y.Map entries ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
           (fun fmt (k, v) -> Format.fprintf fmt "%S: %a" k pp_value v))
        entries

let value_testable = Alcotest.testable pp_value ( = )

let parse_ok input =
  match Y.parse_string input with
  | Ok v -> v
  | Error e -> Alcotest.failf "expected parse success, got error: %s" e

let parse_error input =
  match Y.parse_string input with
  | Ok v -> Alcotest.failf "expected a parse error, got %a" pp_value v
  | Error e -> e

let check_value name expected input =
  Alcotest.check value_testable name expected (parse_ok input)

let contains_sub hay needle =
  let lh = String.length hay in
  let ln = String.length needle in
  if ln = 0 then true
  else
    let rec loop i =
      if i + ln > lh then false
      else if String.sub hay i ln = needle then true
      else loop (i + 1)
    in
    loop 0

let has_line e n = contains_sub e (Printf.sprintf ":%d:" n)

let expect_error name ~line ~needle input =
  let msg = parse_error input in
  Alcotest.(check bool)
    (name ^ ": names source line " ^ string_of_int line)
    true (has_line msg line);
  Alcotest.(check bool)
    (name ^ ": message mentions '" ^ needle ^ "'")
    true (contains_sub msg needle)

let expect_ok_string name expected = function
  | Ok s -> Alcotest.(check string) name expected s
  | Error e -> Alcotest.failf "%s: unexpected error %s" name e

let expect_ok_bool name expected = function
  | Ok b -> Alcotest.(check bool) name expected b
  | Error e -> Alcotest.failf "%s: unexpected error %s" name e

let expect_ok_int name expected = function
  | Ok i -> Alcotest.(check int) name expected i
  | Error e -> Alcotest.failf "%s: unexpected error %s" name e

let expect_ok_list name expected = function
  | Ok items ->
      Alcotest.check value_testable name (Y.List expected) (Y.List items)
  | Error e -> Alcotest.failf "%s: unexpected error %s" name e

let expect_error_contains name needle = function
  | Ok _ -> Alcotest.failf "%s: expected an error, got a value" name
  | Error e ->
      Alcotest.(check bool)
        (name ^ ": message mentions '" ^ needle ^ "'")
        true (contains_sub e needle)

let with_temp contents f =
  let path = Filename.temp_file "nomia-yaml-lite" ".yaml" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let oc = open_out_bin path in
      output_string oc contents;
      close_out oc;
      f path)

(* ------------------------------------------------------------------ *)
(* Accepted scalars                                                   *)
(* ------------------------------------------------------------------ *)

let scalar_tests =
  [
    Alcotest.test_case "integer" `Quick (fun () ->
        check_value "integer" (Y.Int 42) "42");
    Alcotest.test_case "negative integer" `Quick (fun () ->
        check_value "negative integer" (Y.Int (-7)) "-7");
    Alcotest.test_case "float" `Quick (fun () ->
        check_value "float" (Y.Float 1.5) "1.5");
    Alcotest.test_case "float with exponent" `Quick (fun () ->
        check_value "float with exponent" (Y.Float 1500.) "1.5e3");
    Alcotest.test_case "true" `Quick (fun () ->
        check_value "true" (Y.Bool true) "true");
    Alcotest.test_case "false" `Quick (fun () ->
        check_value "false" (Y.Bool false) "false");
    Alcotest.test_case "null word" `Quick (fun () ->
        check_value "null word" Y.Null "null");
    Alcotest.test_case "null tilde" `Quick (fun () ->
        check_value "null tilde" Y.Null "~");
    Alcotest.test_case "plain string" `Quick (fun () ->
        check_value "plain string" (Y.String "hello world") "hello world");
    Alcotest.test_case "plain string with URL" `Quick (fun () ->
        check_value "plain string with URL"
          (Y.String "http://example.test/a")
          "http://example.test/a");
    Alcotest.test_case "single quoted" `Quick (fun () ->
        check_value "single quoted" (Y.String "a b") "'a b'");
    Alcotest.test_case "single quote escape" `Quick (fun () ->
        check_value "single quote escape" (Y.String "it's") "'it''s'");
    Alcotest.test_case "double quote escapes" `Quick (fun () ->
        check_value "double quote escapes" (Y.String "a\nb\t\"c\\")
          "\"a\\nb\\t\\\"c\\\\\"");
    Alcotest.test_case "double quote unicode escape" `Quick (fun () ->
        check_value "double quote unicode escape" (Y.String "\xc3\xa9")
          "\"\\u00e9\"");
    Alcotest.test_case "quoted integer stays a string" `Quick (fun () ->
        check_value "quoted integer stays a string" (Y.String "42") "\"42\"");
    Alcotest.test_case "quoted true stays a string" `Quick (fun () ->
        check_value "quoted true stays a string" (Y.String "true") "'true'");
    Alcotest.test_case "empty input is null" `Quick (fun () ->
        check_value "empty input is null" Y.Null "");
    Alcotest.test_case "comment-only input is null" `Quick (fun () ->
        check_value "comment-only input is null" Y.Null
          "# just a comment\n# and another\n");
  ]

(* ------------------------------------------------------------------ *)
(* Block structures                                                   *)
(* ------------------------------------------------------------------ *)

let structure_tests =
  [
    Alcotest.test_case "nested mapping and sequence" `Quick (fun () ->
        check_value "nested mapping and sequence"
          (Y.Map
             [
               ("id", Y.String "engineering.verify-change");
               ("revision", Y.Int 3);
               ("scope", Y.List [ Y.String "code-change" ]);
               ("final", Y.Bool false);
               ( "enforcement",
                 Y.Map
                   [
                     ("class", Y.String "gate");
                     ( "controls",
                       Y.List
                         [ Y.String "ci-verification"; Y.String "review" ] );
                   ] );
             ])
          {|id: engineering.verify-change
revision: 3
scope: [code-change]
final: false
enforcement:
  class: gate
  controls:
    - ci-verification
    - review
|});
    Alcotest.test_case "sequence of mappings" `Quick (fun () ->
        check_value "sequence of mappings"
          (Y.List
             [
               Y.Map [ ("name", Y.String "alpha"); ("value", Y.Int 1) ];
               Y.Map [ ("name", Y.String "beta"); ("value", Y.Int 2) ];
             ])
          {|- name: alpha
  value: 1
- name: beta
  value: 2
|});
    Alcotest.test_case "nested sequences" `Quick (fun () ->
        check_value "nested sequences"
          (Y.Map
             [
               ( "items",
                 Y.List
                   [
                     Y.List [ Y.String "a"; Y.String "b" ];
                     Y.List [ Y.String "c" ];
                   ] );
             ])
          {|items:
  - - a
    - b
  - - c
|});
    Alcotest.test_case "bare dash opens nested sequence" `Quick (fun () ->
        check_value "bare dash opens nested sequence"
          (Y.Map
             [
               ( "items",
                 Y.List
                   [
                     Y.List [ Y.String "a"; Y.String "b" ];
                   ] );
             ])
          {|items:
  -
    - a
    - b
|});
    Alcotest.test_case "empty value is null when no block follows" `Quick
        (fun () ->
          check_value "empty value is null when no block follows"
            (Y.Map [ ("empty", Y.Null); ("other", Y.Int 1) ])
            {|empty:
other: 1
|});
    Alcotest.test_case "explicit single document marker" `Quick (fun () ->
        check_value "explicit single document marker"
          (Y.Map [ ("a", Y.Int 1) ])
          "---\na: 1\n");
    Alcotest.test_case "quoted mapping key" `Quick (fun () ->
        check_value "quoted mapping key"
          (Y.Map [ ("quoted key", Y.String "value") ])
          "\"quoted key\": value\n");
    Alcotest.test_case "list value of mappings" `Quick (fun () ->
        check_value "list value of mappings"
          (Y.Map
             [
               ( "rules",
                 Y.List
                   [
                     Y.Map
                       [ ("id", Y.String "r1"); ("owner", Y.String "o1") ];
                     Y.Map [ ("id", Y.String "r2"); ("owner", Y.String "o2") ];
                   ] );
             ])
          {|rules:
  - id: r1
    owner: o1
  - id: r2
    owner: o2
|});
  ]

(* ------------------------------------------------------------------ *)
(* Flow sequences                                                     *)
(* ------------------------------------------------------------------ *)

let flow_tests =
  [
    Alcotest.test_case "flow list of scalars" `Quick (fun () ->
        check_value "flow list of scalars"
          (Y.List [ Y.String "a"; Y.String "b"; Y.Int 3; Y.Bool true ])
          "[a, b, 3, true]");
    Alcotest.test_case "empty flow list" `Quick (fun () ->
        check_value "empty flow list" (Y.List []) "[]");
    Alcotest.test_case "flow list quoted elements" `Quick (fun () ->
        check_value "flow list quoted elements"
          (Y.List [ Y.String "a, b"; Y.String "c" ])
          "[\"a, b\", 'c']");
    Alcotest.test_case "flow list trailing comma" `Quick (fun () ->
        check_value "flow list trailing comma"
          (Y.List [ Y.String "a"; Y.String "b" ])
          "[a, b,]");
    Alcotest.test_case "flow list as map value" `Quick (fun () ->
        check_value "flow list as map value"
          (Y.Map [ ("scope", Y.List [ Y.String "x"; Y.String "y" ]) ])
          "scope: [x, y]\n");
    Alcotest.test_case "flow list as sequence item" `Quick (fun () ->
        check_value "flow list as sequence item"
          (Y.List [ Y.List [ Y.String "a"; Y.String "b" ] ])
          "- [a, b]\n");
  ]

(* ------------------------------------------------------------------ *)
(* Block strings                                                      *)
(* ------------------------------------------------------------------ *)

let block_string_tests =
  [
    Alcotest.test_case "literal block" `Quick (fun () ->
        check_value "literal block"
          (Y.Map [ ("text", Y.String "first\nsecond\n") ])
          "text: |\n  first\n  second\n");
    Alcotest.test_case "literal block keeps extra indentation" `Quick (fun () ->
        check_value "literal block keeps extra indentation"
          (Y.Map [ ("text", Y.String "a\n  b\n") ])
          "text: |\n  a\n    b\n");
    Alcotest.test_case "literal block keeps blank lines" `Quick (fun () ->
        check_value "literal block keeps blank lines"
          (Y.Map [ ("text", Y.String "a\n\nb\n") ])
          "text: |\n  a\n\n  b\n");
    Alcotest.test_case "literal block preserves comment-looking lines" `Quick
        (fun () ->
          check_value "literal block preserves comment-looking lines"
            (Y.Map [ ("text", Y.String "# not a comment\nvalue\n") ])
            "text: |\n  # not a comment\n  value\n");
    Alcotest.test_case "folded block" `Quick (fun () ->
        check_value "folded block"
          (Y.Map [ ("text", Y.String "first second\n") ])
          "text: >\n  first\n  second\n");
    Alcotest.test_case "folded block blank line becomes newline" `Quick
        (fun () ->
          check_value "folded block blank line becomes newline"
            (Y.Map [ ("text", Y.String "first\nsecond\n") ])
            "text: >\n  first\n\n  second\n");
    Alcotest.test_case "empty block string" `Quick (fun () ->
        check_value "empty block string"
          (Y.Map [ ("text", Y.String ""); ("next", Y.Int 1) ])
          "text: |\nnext: 1\n");
    Alcotest.test_case "block string as sequence item" `Quick (fun () ->
        check_value "block string as sequence item"
          (Y.List [ Y.String "line one\nline two\n" ])
          "- |\n  line one\n  line two\n");
    Alcotest.test_case "block string under sequence mapping" `Quick (fun () ->
        check_value "block string under sequence mapping"
          (Y.List [ Y.Map [ ("text", Y.String "hello\nworld\n") ] ])
          "- text: |\n    hello\n    world\n");
  ]

(* ------------------------------------------------------------------ *)
(* Comments                                                           *)
(* ------------------------------------------------------------------ *)

let comment_tests =
  [
    Alcotest.test_case "leading and trailing comments" `Quick (fun () ->
        check_value "leading and trailing comments"
          (Y.Map [ ("key", Y.String "value") ])
          "# leading\nkey: value # trailing\n");
    Alcotest.test_case "hash glued to plain scalar is preserved" `Quick
        (fun () ->
          check_value "hash glued to plain scalar is preserved"
            (Y.Map [ ("key", Y.String "value#not-comment") ])
            "key: value#not-comment\n");
    Alcotest.test_case "hash inside double quotes is preserved" `Quick
        (fun () ->
          check_value "hash inside double quotes is preserved"
            (Y.Map [ ("key", Y.String "a # b") ])
            "key: \"a # b\"\n");
    Alcotest.test_case "hash inside single quotes is preserved" `Quick
        (fun () ->
          check_value "hash inside single quotes is preserved"
            (Y.Map [ ("key", Y.String "a # b") ])
            "key: 'a # b'\n");
    Alcotest.test_case "comment between entries and blank lines" `Quick
        (fun () ->
          check_value "comment between entries and blank lines"
            (Y.Map [ ("key", Y.String "value"); ("other", Y.Int 1) ])
            {|key: value

# a comment between entries
other: 1
|});
    Alcotest.test_case "comment after flow list" `Quick (fun () ->
        check_value "comment after flow list"
          (Y.Map [ ("scope", Y.List [ Y.String "a"; Y.String "b" ]) ])
          "scope: [a, b] # comment\n");
    Alcotest.test_case "hash glued inside flow scalar is preserved" `Quick
        (fun () ->
          check_value "hash glued inside flow scalar is preserved"
            (Y.Map [ ("scope", Y.List [ Y.String "a#b" ]) ])
            "scope: [a#b]\n");
    Alcotest.test_case "comment only value is null" `Quick (fun () ->
        check_value "comment only value is null"
          (Y.Map [ ("key", Y.Null) ])
          "key: # only a comment\n");
  ]

(* ------------------------------------------------------------------ *)
(* Rejected input                                                     *)
(* ------------------------------------------------------------------ *)

let error_tests =
  [
    Alcotest.test_case "duplicate mapping key" `Quick (fun () ->
        expect_error "duplicate mapping key" ~line:2 ~needle:"duplicate"
          "a: 1\na: 2\n");
    Alcotest.test_case "duplicate nested mapping key" `Quick (fun () ->
        expect_error "duplicate nested mapping key" ~line:3 ~needle:"duplicate"
          "outer:\n  a: 1\n  a: 2\n");
    Alcotest.test_case "tab indentation" `Quick (fun () ->
        expect_error "tab indentation" ~line:2 ~needle:"tab"
          "a:\n\tb: 1\n");
    Alcotest.test_case "tab after sequence dash" `Quick (fun () ->
        expect_error "tab after sequence dash" ~line:1 ~needle:"tab"
          "-\tvalue\n");
    Alcotest.test_case "unterminated single quote" `Quick (fun () ->
        expect_error "unterminated single quote" ~line:1 ~needle:"unterminated"
          "a: 'oops\n");
    Alcotest.test_case "unterminated double quote" `Quick (fun () ->
        expect_error "unterminated double quote" ~line:1
          ~needle:"unterminated"
          "a: \"oops\n");
    Alcotest.test_case "unterminated flow list" `Quick (fun () ->
        expect_error "unterminated flow list" ~line:1 ~needle:"unterminated"
          "a: [1, 2\n");
    Alcotest.test_case "unterminated flow quote" `Quick (fun () ->
        expect_error "unterminated flow quote" ~line:2 ~needle:"unterminated"
          "a:\n  b: [\"x]\n");
    Alcotest.test_case "anchor" `Quick (fun () ->
        expect_error "anchor" ~line:1 ~needle:"anchor" "a: &x value\n");
    Alcotest.test_case "alias" `Quick (fun () ->
        expect_error "alias" ~line:1 ~needle:"alias" "a: *x\n");
    Alcotest.test_case "tag" `Quick (fun () ->
        expect_error "tag" ~line:1 ~needle:"tag" "a: !t value\n");
    Alcotest.test_case "anchor as mapping key" `Quick (fun () ->
        expect_error "anchor as mapping key" ~line:1 ~needle:"anchor"
          "&x: 1\n");
    Alcotest.test_case "directive" `Quick (fun () ->
        expect_error "directive" ~line:1 ~needle:"directive"
          "%YAML 1.2\na: 1\n");
    Alcotest.test_case "multiple documents" `Quick (fun () ->
        expect_error "multiple documents" ~line:2 ~needle:"document"
          "a: 1\n---\nb: 2\n");
    Alcotest.test_case "inconsistent indentation" `Quick (fun () ->
        expect_error "inconsistent indentation" ~line:3
          ~needle:"indentation"
          "a:\n    b: 1\n  c: 2\n");
    Alcotest.test_case "over-indented sibling" `Quick (fun () ->
        expect_error "over-indented sibling" ~line:2 ~needle:"indentation"
          "a: 1\n b: 2\n");
    Alcotest.test_case "flow mapping" `Quick (fun () ->
        expect_error "flow mapping" ~line:1 ~needle:"flow mapping"
          "a: {b: 1}\n");
    Alcotest.test_case "nested flow list" `Quick (fun () ->
        expect_error "nested flow list" ~line:1 ~needle:"nested flow"
          "a: [[b]]\n");
    Alcotest.test_case "flow mapping syntax inside flow list" `Quick (fun () ->
        expect_error "flow mapping syntax inside flow list" ~line:1
          ~needle:"flow mapping"
          "a: [b: 1]\n");
    Alcotest.test_case "empty mapping key" `Quick (fun () ->
        expect_error "empty mapping key" ~line:1 ~needle:"empty mapping key"
          ": value\n");
    Alcotest.test_case "empty quoted mapping key" `Quick (fun () ->
        expect_error "empty quoted mapping key" ~line:1
          ~needle:"empty mapping key"
          "\"\": value\n");
    Alcotest.test_case "flow list cannot span lines" `Quick (fun () ->
        expect_error "flow list cannot span lines" ~line:1
          ~needle:"unterminated"
          "a: [1,\n  2]\n");
    Alcotest.test_case "sequence item in mapping" `Quick (fun () ->
        expect_error "sequence item in mapping" ~line:2
          ~needle:"sequence item"
          "a: 1\n- b\n");
    Alcotest.test_case "colon space inside plain value" `Quick (fun () ->
        expect_error "colon space inside plain value" ~line:1
          ~needle:"plain scalar"
          "a: b: c\n");
    Alcotest.test_case "inline block sequence entry" `Quick (fun () ->
        expect_error "inline block sequence entry" ~line:1
          ~needle:"sequence entry"
          "a: - b\n");
    Alcotest.test_case "unsupported block scalar header" `Quick (fun () ->
        expect_error "unsupported block scalar header" ~line:1
          ~needle:"block scalar"
          "a: |2\n  x\n");
    Alcotest.test_case "reserved indicator" `Quick (fun () ->
        expect_error "reserved indicator" ~line:1 ~needle:"reserved"
          "a: @bad\n");
    Alcotest.test_case "content after top-level scalar" `Quick (fun () ->
        expect_error "content after top-level scalar" ~line:2
          ~needle:"unexpected content"
          "foo\nbar\n");
    Alcotest.test_case "sequence of scalars cannot hold a mapping" `Quick
        (fun () ->
          expect_error "sequence of scalars cannot hold a mapping" ~line:2
            ~needle:"sequence item"
            "- a\nkey: value\n");
  ]

(* ------------------------------------------------------------------ *)
(* Typed accessors                                                    *)
(* ------------------------------------------------------------------ *)

let accessor_tests =
  let doc =
    parse_ok
      {|name: policy
count: 3
enabled: true
ratio: 1.5
tags: [a, b]
nested:
  key: value
|}
  in
  [
    Alcotest.test_case "get present" `Quick (fun () ->
        Alcotest.check value_testable "get present" (Y.String "policy")
          (match Y.get "name" doc with
          | Some v -> v
          | None -> Y.Null));
    Alcotest.test_case "get absent" `Quick (fun () ->
        Alcotest.(check bool) "get absent" true (Y.get "missing" doc = None));
    Alcotest.test_case "get_string" `Quick (fun () ->
        expect_ok_string "get_string" "policy" (Y.get_string "name" doc));
    Alcotest.test_case "get_string type error" `Quick (fun () ->
        expect_error_contains "get_string type error" "string"
          (Y.get_string "count" doc));
    Alcotest.test_case "get_string missing" `Quick (fun () ->
        expect_error_contains "get_string missing" "missing"
          (Y.get_string "missing" doc));
    Alcotest.test_case "get_list" `Quick (fun () ->
        expect_ok_list "get_list" [ Y.String "a"; Y.String "b" ]
          (Y.get_list "tags" doc));
    Alcotest.test_case "get_list type error" `Quick (fun () ->
        expect_error_contains "get_list type error" "list"
          (Y.get_list "name" doc));
    Alcotest.test_case "get_bool" `Quick (fun () ->
        expect_ok_bool "get_bool" true (Y.get_bool "enabled" doc));
    Alcotest.test_case "get_bool type error" `Quick (fun () ->
        expect_error_contains "get_bool type error" "boolean"
          (Y.get_bool "count" doc));
    Alcotest.test_case "get_int" `Quick (fun () ->
        expect_ok_int "get_int" 3 (Y.get_int "count" doc));
    Alcotest.test_case "get_int type error" `Quick (fun () ->
        expect_error_contains "get_int type error" "integer"
          (Y.get_int "ratio" doc));
    Alcotest.test_case "nested get" `Quick (fun () ->
        let nested =
          match Y.get "nested" doc with
          | Some v -> v
          | None -> Alcotest.fail "nested key missing"
        in
        Alcotest.check value_testable "nested get" (Y.String "value")
          (match Y.get "key" nested with
          | Some v -> v
          | None -> Y.Null));
    Alcotest.test_case "missing accessors" `Quick (fun () ->
        expect_error_contains "get_list missing" "missing"
          (Y.get_list "missing" doc);
        expect_error_contains "get_bool missing" "missing"
          (Y.get_bool "missing" doc);
        expect_error_contains "get_int missing" "missing"
          (Y.get_int "missing" doc));
    Alcotest.test_case "get on non-mapping" `Quick (fun () ->
        Alcotest.(check bool) "get on non-mapping" true
          (Y.get "some" (Y.List []) = None));
  ]

(* ------------------------------------------------------------------ *)
(* parse_file                                                         *)
(* ------------------------------------------------------------------ *)

let parse_file_tests =
  [
    Alcotest.test_case "parse_file success" `Quick (fun () ->
        with_temp "a: 1\n" (fun path ->
            match Y.parse_file path with
            | Ok v ->
                Alcotest.check value_testable "parse_file success"
                  (Y.Map [ ("a", Y.Int 1) ])
                  v
            | Error e -> Alcotest.failf "unexpected parse_file error: %s" e));
    Alcotest.test_case "parse_file reports path and line" `Quick (fun () ->
        with_temp "a: 1\na: 2\n" (fun path ->
            match Y.parse_file path with
            | Ok _ -> Alcotest.fail "expected parse_file to fail"
            | Error e ->
                Alcotest.(check bool) "parse_file reports path" true
                  (contains_sub e path);
                Alcotest.(check bool) "parse_file reports line 2" true
                  (has_line e 2);
                Alcotest.(check bool) "parse_file reports duplicate" true
                  (contains_sub e "duplicate")));
    Alcotest.test_case "parse_file read error" `Quick (fun () ->
        let path = Filename.temp_file "nomia-yaml-lite" ".yaml" in
        Sys.remove path;
        match Y.parse_file path with
        | Ok _ -> Alcotest.fail "expected missing file to fail"
        | Error e ->
            Alcotest.(check bool) "read error mentions file" true
              (contains_sub e "cannot read file"));
  ]

(* ------------------------------------------------------------------ *)
(* Run                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "nomia yaml_lite"
    [
      ("scalars", scalar_tests);
      ("structures", structure_tests);
      ("flow lists", flow_tests);
      ("block strings", block_string_tests);
      ("comments", comment_tests);
      ("errors", error_tests);
      ("accessors", accessor_tests);
      ("parse_file", parse_file_tests);
    ]
