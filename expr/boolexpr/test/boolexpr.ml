open BoolexprLib.Main

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test test_eval("false") = false;;
let%test test_eval("true") = true;;
let%test test_eval("if true then false else true") = false;;

(* ### Unit tests for task 5 *)

let%test "test_trace1_1" = failwith "TODO"
