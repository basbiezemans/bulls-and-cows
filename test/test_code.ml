open OUnit2
open Code

let test_secret _ =
  assert_equal true (is_valid secret)

let test_is_valid _ =
  assert_equal true (is_valid [1; 2; 3; 4]);
  assert_equal true (is_valid [6; 2; 3; 4]);
  assert_equal false (is_valid [1; 2; 3; 7]);
  assert_equal false (is_valid [0; 1; 2; 3]);
  assert_equal false (is_valid [1; 2; 3]);
  assert_equal false (is_valid [1; 2; 3; 4; 5])

let test_code_of_string _ =
  assert_equal true @@ Result.is_error (of_string "");
  assert_equal true @@ Result.is_error (of_string "12");
  assert_equal true @@ Result.is_error (of_string "12e4");
  assert_equal true @@ Result.is_ok (of_string "1234")

let test_code_to_string _ =
  assert_equal "1234" (to_string [1; 2; 3; 4])

let suite =
  "bulls-and-cows" >::: [
    "is_valid" >:: test_is_valid;
    "secret" >:: test_secret;
    "code_of_string" >:: test_code_of_string;
    "code_to_string" >:: test_code_to_string;
  ]

let () = run_test_tt_main suite
