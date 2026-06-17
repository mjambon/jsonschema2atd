open OUnit2
open Base

let top_level_enum _ =
  let input =
    {|{
    "dummy": {
      "type": "string",
      "enum": [
        "success",
        "in_progress",
        "failed"
      ]
    }
  }|}
  in
  let output =
    {|
    type dummy = [
      | Success <json name="success">
      | In_progress <json name="in_progress">
      | Failed <json name="failed">
    ]

  |}
  in
  assert_schema input output

let nested_enum _ =
  let input =
    {|{
    "dummy": {
      "type": "object",
      "properties": {
        "nested": {
          "type": "string",
          "enum": [
            "success",
            "in_progress",
            "failed"
          ]
        }
      }
    }
  }|}
  in
  let output =
    {|
    type dummyNested = [
      | Success <json name="success">
      | In_progress <json name="in_progress">
      | Failed <json name="failed">
    ]

    type dummy = {
      ?nested: dummyNested option;
    }

  |}
  in
  assert_schema input output

let plus_in_enum_value _ =
  (* '+' in enum values must not produce duplicate constructors *)
  let input =
    {|{
    "level": {
      "type": "string",
      "enum": ["eal1", "eal1+", "eal2", "eal2+"]
    }
  }|}
  in
  let output =
    {|
    type level = [
      | Eal1 <json name="eal1">
      | Eal1plus <json name="eal1+">
      | Eal2 <json name="eal2">
      | Eal2plus <json name="eal2+">
    ]
  |}
  in
  assert_schema input output

let suite =
  "Nullable"
  >::: [
         "top level enum" >:: top_level_enum;
         "nested enum" >:: nested_enum;
         "plus in enum value" >:: plus_in_enum_value;
       ]
let () = run_test_tt_main suite
