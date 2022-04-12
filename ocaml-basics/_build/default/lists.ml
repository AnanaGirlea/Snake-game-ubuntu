open Core

(* We can define a list using brackets and semicolons. *)
let xs = [ 1; 2; 3 ]

(* We can also define a list using the "::" (cons) operator. *)
let ys = 1 :: 2 :: 3 :: []

(* The cons operator can be used to prepend an element to a list. *)
let zs = 1 :: [ 2; 3 ]

(* We can match on lists! *)
let print_first_element xs =
  match xs with
  | []     -> print_string "empty"
  | x :: _ -> printf "%d" x
;;

(* There are lots of useful list functions in the [List] module, which we will see later! *)
