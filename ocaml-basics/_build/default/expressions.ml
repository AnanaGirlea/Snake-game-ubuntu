open Core

(* The keyword [let] is used to define variables and functions. *)
let x = 5
let f x = x * 2

(* "let ... ;;" is used to define variables and functions at the toplevel. 
   (The ";;" is usually omitted for single lined definitions.) *)
let my_very_awesome_sentence =
  "if you say it loud enough you'll always sound precocious"
;;

let my_very_awesome_function x =
  x + x + x + x + x + x + x + x + x + x + x + x + x + x
;;

(* "let ... in" is used to define variables and functions within other
   expressions. *)
let double_plus_one x =
  let doubled = x * 2 in
  doubled + 1
;;

(* Everything in OCaml is an expression! *)

(* There is no return keyword. Everything has a value. *)
let two_times_half_rounding_up x =
  let result = 
    if x mod 2 = 0
    then x / 2
    else (x / 2) + 1
  in
  result * 2
;;

(* Things like if statements are also expressions. *)
let three_times_half_rounding_up x =
  3 * 
  (if x mod 2 = 0 
   then x / 2
   else (x / 2) + 1)
;;

