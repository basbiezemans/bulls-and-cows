open Funclib
open OUnit2

let test_secret _ =
  assert_equal true (is_valid secret)

let suite =
  "bulls-and-cows" >::: [
    "secret" >:: test_secret;
  ]

let () = run_test_tt_main suite
