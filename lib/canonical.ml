(** Canonical JSON bytes and content digests. *)

let rec sort (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc entries ->
      `Assoc
        (entries
        |> List.map (fun (key, value) -> (key, sort value))
        |> List.stable_sort (fun (a, _) (b, _) -> String.compare a b))
  | `List items -> `List (List.map sort items)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as scalar ->
      scalar

let rec is_finite (json : Yojson.Safe.t) =
  match json with
  | `Float f -> Float.is_finite f
  | `Assoc entries -> List.for_all (fun (_, value) -> is_finite value) entries
  | `List items -> List.for_all is_finite items
  | `Null | `Bool _ | `Int _ | `Intlit _ | `String _ -> true

let to_string json =
  let json = sort json in
  if not (is_finite json) then
    invalid_arg "Canonical.to_string: NaN and infinity are not valid JSON";
  Yojson.Safe.to_string ~std:true json

let sha256_hex value = Digestif.SHA256.(digest_string value |> to_hex)

let sha256_file path =
  let contents = In_channel.with_open_bin path In_channel.input_all in
  sha256_hex contents

(* Big-endian 64-bit length. An OCaml [int] is at most 63 bits on the
   supported 64-bit targets, so eight bytes are always enough and the value is
   never negative. *)
let length_prefixed value =
  let length = String.length value in
  let framed = Bytes.create (8 + length) in
  let rec put index remaining =
    if index >= 0 then begin
      Bytes.set framed index (Char.chr (remaining land 0xff));
      put (index - 1) (remaining lsr 8)
    end
  in
  put 7 length;
  Bytes.blit_string value 0 framed 8 length;
  Bytes.unsafe_to_string framed

let manifest_prefix = "nomia-manifest-v1\000"

let manifest_digest json =
  "sha256:" ^ sha256_hex (manifest_prefix ^ length_prefixed (to_string json))
