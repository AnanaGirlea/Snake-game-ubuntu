open! Base

module Color = struct
  type t = 
  | Red 
  | Gold [@@deriving sexp_of]
end

type t = { location : Position.t; color : Color.t} [@@deriving sexp_of]

let location t = t.location
let color t = t.color

let color t =
  t.color
;;

let amount_to_grow t =
  match color t with
  | Red -> 2
  | Gold -> 2
;;
   
let create ~board ~snake =
  let elem_board = Board.all_locations board in
  let elem_snake = Snake.all_locations snake in
  let free = List.filter elem_board ~f:(fun x -> not(List.mem elem_snake x ~equal:Position.equal)) in
  (*
  let change_color t =
    if(t.location.row == 1 ) then 
      "Red"
  else "Gold" in 
*)
  match free with
  | [] -> None
  | _ -> Some { location = List.random_element_exn free; color = Gold}
;;

module Exercises = struct
  let exercise05 = create
  let create_with_location location = { location; color = Gold}
end
