open! Base
open! Snake_lib

let%expect_test "Exercise 02a" =
  let test key =
    let direction = Direction.Exercises.exercise02a key in
    Stdio.printf !"%c -> %{sexp: Direction.t option}" key direction
  in
  test 'w';
  [%expect {| w -> () |}];
  test 'a';
  [%expect {| a -> () |}];
  test 's';
  [%expect {| s -> () |}];
  test 'd';
  [%expect {| d -> () |}];
  test 'q';
  [%expect {| q -> () |}];
  test 'e';
  [%expect {| e -> () |}];
  test 'r';
  [%expect {| r -> () |}]
;;
