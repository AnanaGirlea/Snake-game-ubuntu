open! Base

type t =
  { mutable snake : Snake.t
  ; mutable game_state : Game_state.t
  ; mutable apple : Apple.t
  ; board : Board.t
  }
[@@deriving sexp_of]

let to_string { snake; game_state; apple; board } =
  Core.sprintf
    !{|Game state: %{sexp:Game_state.t}
Apple: %{sexp:Apple.t}
Board: %{sexp:Board.t}
Snake:
%s |}
    game_state
    apple
    board
    (Snake.to_string ~indent:2 snake)
;;

let create ~height ~width ~initial_snake_length =
  let board = Board.create ~height ~width in
  let snake = Snake.create ~length:initial_snake_length in
  let apple = Apple.create ~board ~snake in
  match apple with
  | None -> failwith "unable to create initial apple"
  | Some apple ->
    let t = { snake; apple; game_state = In_progress; board } in
    if List.exists (Snake.all_locations snake) ~f:(fun pos ->
           not (Board.in_bounds t.board pos))
    then failwith "unable to create initial snake"
    else t
;;

let snake t = t.snake
let apple t = t.apple
let game_state t = t.game_state


let handle_key t key =
  let new_direction = Direction.of_key key in
  match new_direction with
  | Some new_direction -> Snake.set_direction t.snake new_direction
  | None -> ()
;;

let check_for_collisions t = 
  let collision = Board.in_bounds t.board (Snake.head t.snake) in
  if not collision then
     t.game_state <- Game_over "Out of bounds!"
  else ()
;;

let maybe_consume_apple t = 
  let head = Snake.head t.snake in
  let apple = Apple.location t.apple in
  
  if Position.equal head apple then (
    Snake.grow_over_next_steps  t.snake (Apple.amount_to_grow t.apple);

    match Apple.create ~board:t.board ~snake:t.snake with
    | None -> t.game_state <- Game_state.Win
    | Some apple -> t.apple <- apple
  )
  else ()
;;

let step t =
  if Snake.step t.snake
  then (
    check_for_collisions t;
    maybe_consume_apple t)
  else 
    t.game_state <- Game_over "Self collision!"
;;

module Exercises = struct
  let exercise02b = handle_key

  let exercise03b t snake =
    let t = { t with snake } in
    check_for_collisions t;
    t.game_state
  ;;

  let exercise04b t snake =
    let t = { t with snake } in
    step t;
    t.snake, t.game_state
  ;;

  let exercise06b = maybe_consume_apple
  let set_apple t apple = t.apple <- apple
  let set_snake t snake = t.snake <- snake
end
