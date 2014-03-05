

type json_string = string

type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string ] 

type primitive = [
  | `Null
  | `Boolean
  | `Int (* 32 bit signed *)
  | `Long (* 64 bit signed *)
  | `Float (* single precision *)
  | `Double (* double precision *)
  | `Bytes (* sequence of 8-bit unsigned *)
  | `String (* unicode string *)
]
type literal_value = [
  | `Raw of string
  | `Json of json
]
type record_field = {
  field_name: json_string;
  field_doc: json_string option; 
  field_type: t; (* TODO spec not totally clear if type can be Union _ *)
  field_default: literal_value option;
  field_order: [ `Ascending | `Descending | `Ingnore ] option;
  field_aliases: json_string option;
}
and record_type = {
  record_name : json_string;
  record_namespace: json_string;
  record_doc: json_string option; 
  record_aliases: json_string list;
  record_fields: record_field list;
}
and enum_type = {
  enum_name: json_string;
  enum_namespace: json_string;
  enum_doc: json_string option; 
  enum_aliases: json_string list;
  enum_symbols: json_string list;
}
and fixed_type = {
  fixed_name: json_string;
  fixed_namespace: json_string;
  fixed_aliases: json_string list;
  fixed_size: int;
}
and object_description = [
  | primitive
  | `Record of record_type
  | `Enum of enum_type
  | `Array of t list
  | `Map of t list
  | `Union of t list (* this does not encore that unions cannot contain unions *)
  | `Fixed of fixed_type
]
and t = [
  | `Named_type of string
  | `Object of object_description * (string * string) list
  | `Union of t list
]

module Result = struct
  type ('a, 'b) t = [
    | `Ok of 'a
    | `Error of 'b
  ]
  let return a : (_, _) t = `Ok a
  let fail b : (_, _) t = `Error b
  let bind x f : (_, _) t = match x with `Ok a -> f a | `Error _ as e -> e
  let (>>=) = bind
end
open Result

let validate_name s =
  let module With_exn = struct
    exception Wrong_char of char
    let f s =
      try
        begin match s.[0] with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> ()
        | c -> raise (Wrong_char c)
        end;
        String.iter (function
          | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> ()
          | c -> raise (Wrong_char c)) s;
        return s
      with e -> `Error (`Invalid_avro_name s)
  end in
  With_exn.f s

let of_json json =
  match json with
  | `Assoc l -> assert false
  | `List l -> assert false
  | `String s ->
    validate_name s
    >>= fun name ->
    return (`Named_type name)
  | `Bool _
  | `Float _
  | `Int _
  | `Null -> fail (`Unexpected_json json)

open Printf
let error_to_string ~json_to_string = function
| `Unexpected_json j -> sprintf "(Unexpected_json %s)" (json_to_string j)
| `Invalid_avro_name n -> sprintf "(Invalid_avro_name %S)" n

