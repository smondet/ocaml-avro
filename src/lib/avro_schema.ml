

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


