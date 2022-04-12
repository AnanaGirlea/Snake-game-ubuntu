open! Core

(* There are six primitive types. *)
let (a : int) = 5
let (b : float) = 5.0
let (c : bool) = false
let (d : char) = 'd'
let (e : string) = "eeee"

(* We'll get back to this one *)
let (f : unit) = ()

(* We can define types using the "type" keyword. *)
type my_int = int

let (g : my_int) = 6

(* We use "*" to define tuple types. *)
type two_tuple = int * int

let (h : two_tuple) = 2, 3

(* We can define arbitrarily-lengthed tuples in OCaml. *)
type four_tuple = int * int * int * int

let (i : four_tuple) = 1, 2, 3, 4

(* We can pattern match on tuples to unpack their values. *)
let sum my_four_tuple =
  let a, b, c, d = my_four_tuple in
  a + b + c + d
;;

(* OCaml supports some useful algebraic data types, like records and
   variants. *)

(* Here's the syntax for defining a record. *)
type personal_info =
  { age : int
  ; name : string
  ; height : float
  }

(* Here's the syntax for defining a variant. *)
type animal =
  | Dog
  | Cat
  | Lion

(* Variants can have additional arguments as well. *)
type creature =
  | Animal        of animal
  | Animal_hybrid of animal * animal
  | Person        of personal_info
  | Other         of string
