open Printf

let json_of_string s =
  Yojson.Basic.from_string s
let json_to_string s =
  Yojson.Basic.to_string s

let test_failures : string list ref = ref []
let fail_test s = test_failures := s :: !test_failures
let fail_testf fmt = Printf.ksprintf fail_test fmt

let string_test_ok ~json expected =
  begin match Avro_schema.of_json (json_of_string json) with
  | `Ok o -> if o = expected then () else fail_test json
  | `Error e ->
    fail_testf "%s → Error %s" json 
      (Avro_schema.error_to_string ~json_to_string e)
  end
let json_test_ok ~json expected =
  begin match Avro_schema.of_json json with
  | `Ok o -> if o = expected then () else fail_test (json_to_string json)
  | `Error e ->
    fail_testf "%s → Error %s" (json_to_string json)
      (Avro_schema.error_to_string ~json_to_string e)
  end

let string_test_error ~json expected =
  begin match Avro_schema.of_json (json_of_string json) with
  | `Ok o -> fail_testf "Got OK: %s" json
  | `Error e when e = expected -> ()
  | `Error e ->
    fail_testf "%s → Error %s" json 
      (Avro_schema.error_to_string ~json_to_string e)
  end
let json_test_error ~json expected =
  begin match Avro_schema.of_json json with
  | `Ok o -> fail_testf "Got OK: %s" (json_to_string json)
  | `Error e when e = expected -> ()
  | `Error e ->
    fail_testf "%s → Error %s" (json_to_string json) 
      (Avro_schema.error_to_string ~json_to_string e)
  end

module Cons = struct
  (* A curated version should go to the library itself *)

  open Avro_schema

  let record ~name ~namespace ?doc ?(aliases=[]) ?(add=[]) fields =
    `Record {record_name = name;
             record_namespace = namespace;
             record_doc = doc;
             record_aliases = aliases;
             record_fields = fields;
             record_additional = add;
            }

  let field 
      ?doc ?default_value ?order ?(aliases=[]) ?(add=[]) (name, avro_type) =
    {
      field_name = name;
      field_doc = doc;
      field_type = avro_type;
      field_default = default_value;
      field_order = order;
      field_aliases = aliases;
      field_additional = add;
    }

end

let () =
  (* Some “whole parsings” tests, string -> Avro *) 
  string_test_ok ~json:"\"some_json_string\"" (`Named_type "some_json_string");
  string_test_error ~json:"\"0some_json_string\"" (`Invalid_avro_name "0some_json_string");
  string_test_error ~json:"0" (`Unexpected_json ("toplevel", `Int 0));
  string_test_ok ~json:"[\"one\", \"int\"]" (`Union [`Named_type "one"; `Int]);
  string_test_ok ~json:"{ \"type\": \"someName\" }" 
    (`Object (`Named_type "someName", []));
  (* Now with JSON AST as input (i.e. the actual parsing done by the library) *)
  let assoc l = (`Assoc l : Yojson.Basic.json) in
  (* `json_test_ok` checks some Yojson.Basic.json against some Avro schema: *)
  json_test_ok ~json:(assoc ["type", `String "someName"; "attr_one", `String "attr_one_value"])
    (`Object (`Named_type "someName", ["attr_one", `String "attr_one_value"]));
  let json_record ?name ?namespace ?doc ?aliases more_attr =
    (* Build potentially erroneous JSON for an Avro record *)
    let string_field k v = (k, `String v) in 
    let array_of_strings_field k v =
      (k, `List (List.map (fun s -> `String s) v)) in
    let add_option opt f l =
      match opt with
      | Some n -> f n :: l
      | None -> l in
    let actual_attr =
      more_attr
      |> add_option namespace (string_field "namespace")
      |> add_option name (string_field "name")
      |> add_option doc (string_field "doc")
      |> add_option aliases (array_of_strings_field "aliases")
    in
    assoc (("type", `String "record") :: actual_attr)
  in
  (* Test records without fields: *)
  let name = "SomeName" in
  let namespace = "SomeNamespace" in
  json_test_ok ~json:(json_record ~name ~namespace [])
    (Cons.record ~name ~namespace []);
  json_test_ok ~json:(json_record ~name ~namespace ~doc:"DOC" [])
    (Cons.record ~name ~namespace ~doc:"DOC" []);
  json_test_ok ~json:(json_record ~name ~namespace ~doc:"DOC" ~aliases:["bouh"; "bah"] [])
    (Cons.record ~name ~namespace ~doc:"DOC" ~aliases:["bouh"; "bah"] []);
  (* Records without fields but errors: *)
  let wrong_json_for_aliases = `String "someString" in
  json_test_error ~json:(
    json_record ~name ~namespace ~doc:"DOC" ["aliases", wrong_json_for_aliases])
    (`Unexpected_json ("record.aliases", wrong_json_for_aliases));
  let wrong_json_for_doc = `List [`String "someString"] in
  json_test_error ~json:(
    json_record ~name ~namespace ["doc", wrong_json_for_doc])
    (`Unexpected_json ("record.doc", wrong_json_for_doc));
  (* Now to test fields, let's get some decent JSON and its corresponding avro
     as basis: *)
  let good_json_record fields =
    json_record ~name ~namespace ~doc:"DOC" ~aliases:["bouh"; "bah"] fields in
  let good_record_parsed fields =
    Cons.record ~name ~namespace ~doc:"DOC" ~aliases:["bouh"; "bah"] fields in
  json_test_ok (good_json_record []) (good_record_parsed []);
  json_test_ok 
    (good_json_record ["fields", `List []])
    (good_record_parsed []);
  json_test_ok 
    (good_json_record ["fields", `List [`Assoc ["name", `String "FieldName1"; "type", `String "string"]]])
    (good_record_parsed [
        Cons.field ("FieldName1", `String)
        (* ?doc ?default_value ?order ?(aliases=[]) ?(add=[]) (name, avro_type) *)
      ]);


  begin match test_failures with
  | {contents = []} -> print_string "Tests OK\n"
  | {contents} ->
    print_string "Some tests failed\n\n";
    List.iter (printf "- %s\n") contents;
    exit 1
  end
