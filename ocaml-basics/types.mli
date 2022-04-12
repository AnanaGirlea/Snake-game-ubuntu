val a : int
val b : float
val c : bool
val d : char
val e : string
val f : unit

type my_int

val g : my_int

type two_tuple

val h : two_tuple

type four_tuple

val i : four_tuple
val sum : four_tuple -> int

type personal_info =
  { age : int
  ; name : string
  ; height : float
  }

type animal =
  | Dog
  | Cat
  | Lion

type creature =
  | Animal        of animal
  | Animal_hybrid of animal * animal
  | Person        of personal_info
  | Other         of string
