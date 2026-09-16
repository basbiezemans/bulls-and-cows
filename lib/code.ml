open Batteries

let in_range n range =
  let (min, max) = range in min <= n && n <= max

let is_valid code =
  let in_range = Fun.flip in_range in
  List.length code = 4 && List.for_all (in_range (1, 6)) code

let int_of_digit c =
  if in_range c ('0', '9') then
    Some (Char.code c - Char.code '0')
  else
    None

let of_string s =
  if String.length s < 4 then
    Error "please enter 4 digits"
  else
    let code = String.sub s 0 4
      |> String.to_list
      |> List.map int_of_digit
      |> List.map (Option.default 0) in
    if is_valid code then
      Ok code
    else
      Error "each digit should be between 1 and 6, e.g. 1236"

(**
 * [random n range lst] generates a list of [n] random integers in the
 * [range] (min, max).
 * @param n number of random integers
 * @param range the min..max range (inclusive)
 * @param lst an initial list
 * @return a list of random integers
 *)
let rec random n range lst =
  if List.length lst >= n then
    lst
  else
    let (min, max) = range in
    random n range (Random.int_in_range ~min:min ~max:max :: lst)

let secret = random 4 (1, 6) []

let to_string = Fun.compose (String.concat "") (List.map Int.to_string)