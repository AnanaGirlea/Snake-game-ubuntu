open Core

(* Modules are a way of organizing types, variables, and functions. *)
module My_module = struct
  type t = int

  let my_module_value = 5
end

(* You can access values inside the module by using "." *)
let value_inside_module = My_module.my_module_value

(* We can define signatures for modules. *)
module My_module2 : sig
  val x : int
  val f : int -> int
end = struct
  let x = 5
  let f y = y * 2
end

(* Files are modules too! For example, we can access values and functions inside functions.ml. *)
let z = Functions.add_three_ints 1 2 3

(* Just like how we defined a signature for [My_module2] above, we can define a
   signature for a file in the "interface file", which has the .mli extension. 
   See modules.mli *)
