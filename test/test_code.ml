open OUnit2
open Code

let test_code_of_string _ =
  assert_equal true @@ Result.is_error (of_string "");
  assert_equal true @@ Result.is_error (of_string "12");
  assert_equal true @@ Result.is_error (of_string "12e4");
  assert_equal true @@ Result.is_error (of_string "12345");
  assert_equal true @@ Result.is_ok (of_string "1234")

let test_code_to_string _ =
  assert_equal "1234" (to_string (D1, D2, D3, D4))

let test_code_to_list _ =
  assert_equal [D1; D2; D3; D4] (to_list (D1, D2, D3, D4))

let suite =
  "bulls-and-cows" >::: [
    "code_of_string" >:: test_code_of_string;
    "code_to_string" >:: test_code_to_string;
    "code to list" >:: test_code_to_list;
  ]

let () = run_test_tt_main suite
