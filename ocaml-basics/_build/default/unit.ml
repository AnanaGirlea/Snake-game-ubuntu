open Core

(* The unit is a special type, the return type of side-effecting code. *)
let print_hello name = printf "hello %s" name

let print_hello_twice name =
  let () = print_hello name in
  print_hello name
;;

(* We can use ";" to sequence multiple side-effects. *)
let print_hello_thrice name =
  print_hello name;
  print_hello name;
  print_hello name
;;
