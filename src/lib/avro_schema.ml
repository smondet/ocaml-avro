

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
type additional_attribute = string * json
type record_field = {
  field_name: json_string;
  field_doc: json_string option; 
  field_type: object_description; (* TODO spec not totally clear if type can be Union _ *)
  field_default: literal_value option;
  field_order: [ `Ascending | `Descending | `Ingnore ] option;
  field_aliases: json_string list;
  field_additional: additional_attribute list;
  (* We keep everything, as additional attributes *)
}
and record_type = {
  record_name : json_string;
  record_namespace: json_string;
  record_doc: json_string option; 
  record_aliases: json_string list;
  record_fields: record_field list;
  record_additional: additional_attribute list;
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
  | `Array of object_description list
  | `Map of object_description list
  | `Union of object_description list
  (* this does not encode that unions should not contain unions *)
  | `Fixed of fixed_type
  | `Named_type of string
  | `Object of object_description * (string * json) list
]
(* `t` is the allowed “top-level” subset of `object_description` *)
and t = [
  | `Named_type of string
  | `Object of object_description * (string * json) list
  (* actual objet × optional attributes *)
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
  let required ~error o = match o with None -> `Error error | Some o -> `Ok o
end
open Result

module List = struct
  include ListLabels
  let map l ~f = rev (rev_map ~f l)
  let fold = fold_left
end

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

let identify_type s =
  match s with  
  | "boolean" -> `Boolean
  | "int" -> `Int (* 32 bit signed *)
  | "long" -> `Long (* 64 bit signed *)
  | "float" -> `Float (* single precision *)
  | "double" -> `Double (* double precision *)
  | "bytes" -> `Bytes (* sequence of 8-bit unsigned *)
  | "string" -> `String (* unicode string *)
  | other -> `Named_type other

let string_field reference ~name ~continue json =
  match json with
  | `String s -> reference := Some s; return continue
  | other -> fail (`Unexpected_json (name, other))

let array_of_strings ~name json =
  match json with
  | `List array_of_strings ->
    List.fold array_of_strings ~init:(return []) ~f:(fun prev x ->
        prev >>= fun prev ->
        match x with 
        | `String s -> return (s :: prev)
        | other -> fail (`Unexpected_json (name, other)))
    >>= fun alias_list ->
    return (List.rev alias_list)
  | other -> fail (`Unexpected_json (name, other))

(* Here we begin a bunch of mutally recursive functions *)
let rec parse_record_field l =
  let name = ref None in
  let doc = ref None in
  let _type = ref None in
  let default = ref None in
  let order = ref None in
  let aliases = ref [] in
  List.fold l ~init:(return []) ~f:(fun prev_m att ->
      prev_m >>= fun prev ->
      match att with
      | "name", json ->
        string_field name ~name:"record-field.name" ~continue:prev json 
      | "doc", json ->
        string_field doc ~name:"record-field.doc" ~continue:prev json 
      | "aliases", json ->
        array_of_strings ~name:"record-field.aliases" json
        >>= fun alias_list ->
        aliases := alias_list;
        return prev
      | "default", json -> default := Some (`Json json); return prev
      | "type", json_type -> 
        parse_type json_type
        >>= fun t ->
        _type := Some t;
        return prev
      | "order", json -> assert false
      | some_thing_else -> return (some_thing_else :: prev))
    (*
      | unknown, json ->
        fail (`Unexpected_json ("record-field", `Assoc [unknown, json])))
*)
  >>= fun remaining_attributes ->
  required !name ~error:(`Missing ("record_field_name"))
  >>= fun field_name ->
  required !_type ~error:(`Missing ("record_field_type"))
  >>= fun field_type ->
  return {
    field_name;
    field_doc = !doc;
    field_type;
    field_default = !default;
    field_order = !order;
    field_aliases = !aliases;
    field_additional = remaining_attributes;
  }

and parse_record_type attr = 
  let name = ref None in
  let namespace = ref None in
  let doc = ref None in
  let aliases = ref [] in
  let fields = ref [] in
  List.fold attr ~init:(return []) ~f:(fun prev_m att ->
      prev_m >>= fun prev ->
      match att with
      | "name", `String n -> name := Some n; return prev
      | "name", other -> fail (`Unexpected_json ("record.name", other))
      | "namespace", `String n -> namespace := Some n; return prev
      | "namespace", other -> fail (`Unexpected_json ("record.namespace", other))
      | "doc", `String  d -> doc := Some d; return prev
      | "doc", other -> fail (`Unexpected_json ("record.doc", other))
      | "aliases", `List array_of_strings ->
        List.fold array_of_strings ~init:(return []) ~f:(fun prev x ->
            prev >>= fun prev ->
            match x with 
            | `String s -> return (s :: prev)
            | other -> fail (`Unexpected_json ("record.alias", other)))
        >>= fun alias_list ->
        aliases := List.rev alias_list;
        return prev
      | "aliases", other -> fail (`Unexpected_json ("record.aliases", other))
      | "fields", `List json_fields ->
        List.fold json_fields ~init:(return []) 
          ~f:(fun prev_result field_description ->
              prev_result >>= fun prev ->
              match field_description with
              | `Assoc l -> 
                parse_record_field l
                >>= fun field ->
                return (field :: prev)
              | other -> fail (`Unexpected_json ("record.fields", other)))
        >>= fun parsed_fields ->
        fields := List.rev parsed_fields;
        return prev
      | "fields", other -> fail (`Unexpected_json ("record.fields", other))
      | some_thing_else -> return (some_thing_else :: prev))
  >>= fun remaining_attributes ->
  required !name ~error:(`Missing ("record_name"))
  >>= fun record_name ->
  required !namespace ~error:(`Missing ("record_namespace"))
  >>= fun record_namespace ->
  return {
    record_name; record_namespace;
    record_doc = !doc;
    record_aliases = !aliases;
    record_fields = !fields;
    record_additional = remaining_attributes;
  }

and parse_type json =
  match json with
  | `Assoc (("type", `String "record") :: attr) -> 
    parse_record_type attr
    >>= fun record ->
    return (`Record record)
  | `Assoc (("type", `String t) :: attr) -> 
    validate_name t
    >>= fun name ->
    return (`Object (identify_type name, attr))
  | `List l ->
    List.fold l ~init:(return [])
      ~f:(fun prev x -> 
          prev >>= fun l -> parse_type x >>= fun j -> return (j :: l)) 
    >>= fun union ->
    return (`Union (List.rev union))
  | `String s ->
    validate_name s
    >>= fun name ->
    return (identify_type name)
  | other -> assert false (* TODO *)


let of_json json =
  match json with
  | `Assoc (("type", _) :: _) | `List _ | `String _ ->
    parse_type json
      (*
  | `Assoc (("type", `String "record") :: attr) -> 
    parse_record_type attr
    >>= fun record ->
    return (`Record record)
  | `Assoc (("type", `String t) :: attr) -> 
    validate_name t
    >>= fun name ->
    return (`Object (identify_type name, attr))
  | `List l ->
    List.fold l ~init:(return [])
      ~f:(fun prev x -> prev >>= fun l -> of_json x >>= fun j -> return (j :: l)) 
    >>= fun union ->
    return (`Union (List.rev union))
  | `String s ->
    validate_name s
    >>= fun name ->
    return (identify_type name)
*)
  | `Assoc _
  | `Bool _
  | `Float _
  | `Int _
  | `Null -> fail (`Unexpected_json ("toplevel", json))

open Printf
let error_to_string ~json_to_string = function
| `Unexpected_json (msg, j) -> sprintf "(Unexpected_json %s: %s)" msg (json_to_string j)
| `Invalid_avro_name n -> sprintf "(Invalid_avro_name %S)" n
| `Missing thing -> sprintf "(Missing %s)" thing

