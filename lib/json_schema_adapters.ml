module Or_ref = struct
  (* In JSON Schema draft-07, a $ref overrides any sibling keywords. We treat
     any object containing "$ref" as a pure reference, ignoring other fields. *)
  let normalize = function
    | `Assoc kvs when List.mem_assoc "$ref" kvs ->
        `List [ `String "Ref"; List.assoc "$ref" kvs ]
    | obj -> `List [ `String "Obj"; obj ]

  let restore = function
    | `List [ `String "Ref"; ref ] -> `Assoc [ "$ref", ref ]
    | `List [ `String "Obj"; obj ] -> obj
    | x -> x
end

module Or_bool = struct
  let normalize : Yojson.Safe.t -> Yojson.Safe.t = function
    | `Bool b -> `List [ `String "Bool"; `Bool b ]
    | obj -> `List [ `String "Obj"; obj ]

  let restore = function
    | `List [ `String "Bool"; `Bool b ] -> `Bool b
    | `List [ `String "Obj"; obj ] -> obj
    | x -> x
end

module Ref = Utils.Fresh (String) ()
