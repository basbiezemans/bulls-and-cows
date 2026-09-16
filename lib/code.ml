open Batteries

type digit = D1 | D2 | D3 | D4 | D5 | D6

type code = (digit * digit * digit * digit)

let to_list (a, b, c, d) = [a; b; c; d]

let char_of_digit = function
  | D1 -> '1'
  | D2 -> '2'
  | D3 -> '3'
  | D4 -> '4'
  | D5 -> '5'
  | D6 -> '6'

let digit_of_char = function
  | '1' -> Some D1
  | '2' -> Some D2
  | '3' -> Some D3
  | '4' -> Some D4
  | '5' -> Some D5
  | '6' -> Some D6
  |  _  -> None

let of_string s =
  if String.length s <> 4 then
    Error "please enter 4 digits"
  else
    match String.to_list s |> List.map digit_of_char with
    | [Some a; Some b; Some c; Some d] ->
        Ok (a, b, c, d)
    | _ ->
        Error "each digit should be between 1 and 6, e.g. 1236"

let random_digit () =
  match Random.int_in_range ~min:1 ~max:6 with
  | 1 -> D1
  | 2 -> D2
  | 3 -> D3
  | 4 -> D4
  | 5 -> D5
  | 6 -> D6
  | _ -> assert false

let secret () =
  (random_digit (), random_digit (), random_digit (), random_digit ())

let to_string code =
  code
  |> to_list
  |> List.map char_of_digit
  |> String.of_list