open Printf

let json_of_string s =
  Yojson.Basic.from_string s
let json_to_string s =
  Yojson.Basic.to_string s

let test_failures : string list ref = ref []
let fail_test s = test_failures := s :: !test_failures
let fail_testf fmt = Printf.ksprintf fail_test fmt

let test_ok ~json expected =
  begin match Avro_schema.of_json (json_of_string json) with
  | `Ok o -> if o = expected then () else fail_test json
  | `Error e ->
    fail_testf "%s → Error %s" json 
      (Avro_schema.error_to_string ~json_to_string e)
  end
let test_error ~json expected =
  begin match Avro_schema.of_json (json_of_string json) with
  | `Ok o -> fail_testf "Got OK: %s" json
  | `Error e when e = expected -> ()
  | `Error e ->
    fail_testf "%s → Error %s" json 
      (Avro_schema.error_to_string ~json_to_string e)
  end

let () =
  test_ok ~json:"\"some_json_string\"" (`Named_type "some_json_string");
  test_error ~json:"\"0some_json_string\"" (`Invalid_avro_name "0some_json_string");
  test_error ~json:"0" (`Unexpected_json (`Int 0));
  begin match test_failures with
  | {contents = []} -> print_string "Tests OK\n"
  | {contents} ->
    print_string "Some tests failed\n\n";
    List.iter (printf "- %s\n") contents;
    exit 1
  end
