open OUnit2
open Funclib

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
      let code1 = Result.get_ok (Code.of_string str1) in
      let code2 = Result.get_ok (Code.of_string str2) in
      assert_equal expected (feedback code1 code2)
    )
    test_cases |> ignore (* run the asserts but ignore the result *)

let test_show_feedback _ =
  assert_equal "" (show { bulls = 0; cows = 0 });
  assert_equal "●○○" (show { bulls = 1; cows = 2 })

let suite =
  "bulls-and-cows" >::: [
    "feedback" >:: test_feedback;
    "show" >:: test_show_feedback;
  ]

let () = run_test_tt_main suite
