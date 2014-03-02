
type object_description = unit
type t =
  | Named_type of string
  | Object of object_description * (string * string) list
