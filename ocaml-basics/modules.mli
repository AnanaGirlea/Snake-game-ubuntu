(** This is an interface file. *)

module My_module : sig
  type t

  val my_module_value : t
end

val value_inside_module : My_module.t

module My_module2 : sig
  val x : int
  val f : int -> int
end

val z : int
