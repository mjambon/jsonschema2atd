open OUnit2
open Base

let simple_test _ =
  let input =
    {|{
  "title": "dummy",
  "anyOf": [
    {"type": "string"},
    {"type": "integer"}
  ]
}|}
  in
  let output =
    {|type dummy = [
  | String of string
  | Int of int
] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">|}
  in
  assert_jsonschema input output

let with_refs_test _ =
  let input =
    {|{
  "type": "object",
  "definitions": {
    "linkDoc": {"type": "string"},
    "linkElem": {"type": "integer"}
  },
  "properties": {
    "link": {
      "anyOf": [
        {"$ref": "#/definitions/linkDoc"},
        {"$ref": "#/definitions/linkElem"}
      ]
    }
  }
}|}
  in
  let output =
    {|type rootLink = [
  | LinkDoc of linkDoc
  | LinkElem of linkElem
] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
type linkElem = int
type linkDoc = string
type root = {
  ?link : rootLink option;
}|}
  in
  assert_jsonschema input output

let mixed_ref_and_title_test _ =
  (* $ref with sibling keywords: the $ref wins (JSON Schema draft-07 semantics) *)
  let input =
    {|{
  "type": "object",
  "definitions": {
    "myType": {"type": "string"}
  },
  "properties": {
    "link": {
      "anyOf": [
        {"title": "A String", "$ref": "#/definitions/myType"},
        {"type": "integer"}
      ]
    }
  }
}|}
  in
  let output =
    {|type rootLink = [
  | MyType of myType
  | Int of int
] <json adapter.ocaml="Jsonschema2atd_runtime.Adapter.One_of">
type myType = string
type root = {
  ?link : rootLink option;
}|}
  in
  assert_jsonschema input output

let unknown_format_test _ =
  (* Unknown format values must not cause a crash; the field is treated as string *)
  let input =
    {|{
  "title": "url",
  "type": "string",
  "format": "iri-reference"
}|}
  in
  let output = {|type url = string|} in
  assert_jsonschema input output

let additional_properties_false_test _ =
  (* additionalProperties: false must not cause a crash *)
  let input =
    {|{
  "type": "object",
  "additionalProperties": false,
  "properties": {
    "name": {"type": "string"}
  }
}|}
  in
  let output =
    {|type root = {
  ?name: string option;
}|}
  in
  assert_jsonschema input output

let suite =
  "AnyOf"
  >::: [
         "simple" >:: simple_test;
         "with refs" >:: with_refs_test;
         "mixed $ref and title" >:: mixed_ref_and_title_test;
         "unknown format" >:: unknown_format_test;
         "additionalProperties false" >:: additional_properties_false_test;
       ]

let () = run_test_tt_main suite
