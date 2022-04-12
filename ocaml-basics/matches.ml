(* We've already seen many examples of match statements. Here are some more examples. *)
let is_person (creature : Types.creature) =
  match creature with
  | Animal _        -> false
  | Animal_hybrid _ -> false
  | Other _         -> false
  | Person _        -> true
;;

(* We can sometimes combine match cases that have the same result. *)
let is_person_concise (creature : Types.creature) =
  match creature with
  | Animal _ | Animal_hybrid _ | Other _ -> false
  | Person _ -> true
;;

(* We can use a wildcard to match all cases that are explicitly specified, but this is sometimes dangerous. *)
let is_person_with_wildcard (creature : Types.creature) =
  match creature with
  | Person _ -> true
  | _        -> false
;;
