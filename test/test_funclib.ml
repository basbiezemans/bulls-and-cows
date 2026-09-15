open Funclib
open OUnit2

let test_secret _ =
  assert_equal true (is_valid secret)

let test_in_range _ =
  assert_equal true (in_range 1 (1, 5));
  assert_equal true (in_range 3 (1, 5));
  assert_equal true (in_range 5 (1, 5));
  assert_equal false (in_range 4 (5, 15));
  assert_equal false (in_range 16 (5, 15))

let test_is_valid _ =
  assert_equal true (is_valid [1; 2; 3; 4]);
  assert_equal true (is_valid [6; 2; 3; 4]);
  assert_equal false (is_valid [1; 2; 3; 7]);
  assert_equal false (is_valid [0; 1; 2; 3]);
  assert_equal false (is_valid [1; 2; 3]);
  assert_equal false (is_valid [1; 2; 3; 4; 5])

let test_feedback _ =
  let test_cases = [
    ("1234", "1234", { bulls = 4; cows = 0 });
    ("6243", "6225", { bulls = 2; cows = 0 });
    ("5256", "2244", { bulls = 1; cows = 0 });
    ("1111", "2222", { bulls = 0; cows = 0 });
    ("6423", "2252", { bulls = 0; cows = 1 });
    ("6443", "4124", { bulls = 0; cows = 2 });
    ("6163", "1136", { bulls = 1; cows = 2 });
    ("1234", "2134", { bulls = 2; cows = 2 })] in
  List.map
    (fun (str1, str2, expected) ->
      let code1 = string_to_code str1 in
      let code2 = string_to_code str2 in
      assert_equal expected (feedback code1 code2)
    )
    test_cases |> ignore (* run the asserts but ignore the result *)

let test_show_feedback _ =
  assert_equal "" (show { bulls = 0; cows = 0 });
  assert_equal "●○○" (show { bulls = 1; cows = 2 })

let test_string_to_code _ =
  assert_equal [1; 2; 3; 4] (string_to_code "1234")

let test_code_to_string _ =
  assert_equal "1234" (code_to_string [1; 2; 3; 4])

let suite =
  "bulls-and-cows" >::: [
    "in_range" >:: test_in_range;
    "is_valid" >:: test_is_valid;
    "secret" >:: test_secret;
    "feedback" >:: test_feedback;
    "show" >:: test_show_feedback;
    "string_to_code" >:: test_string_to_code;
    "code_to_string" >:: test_code_to_string;
  ]

let () = run_test_tt_main suite
