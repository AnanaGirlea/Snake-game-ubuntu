open Core

(* To define a function, we use the syntax:
 
   let function_name arg1 arg2 = ... *)
let add x y = x + y

(* To call a function, we use the syntax:

   function_name arg1 arg2 *)
let double x = add x x

(* Note that we don't need any additional parentheses unless
   used to deliminate arguments. *)
let add_three_ints x y z = add x (add y z)

(* We can define named arguments using "~" *)
let divide ~numerator ~denominator = numerator / denominator

(* We invoke functions that have named arguments using "~" as well. *)
let divide_by_two x = divide ~numerator:x ~denominator:2

(* Named arguments can be passed in any order! *)
let divide_by_three x = divide ~denominator:3 ~numerator:x

(* We use the "rec" keyword when defining recursive functions. *)
let rec factorial x =
  if x = 0
  then 1
  else x * (factorial (x - 1))
;;
